{-| This module provides functions that can be used to run queries built using Qdsl functions.
    To use these functions, splice is required, and because of this, sometimes you will have to separate queries
    into a separate module.
    Also, functions after splice need to be passed a connection (except showQuery).

-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
module Haskquill (runQuery, runAction, runSingle, showQuery) where
import Language.Haskell.TH.Syntax
    ( Code(examineCode),
      Q,
      liftCode,
      TExp(..),
      reportWarning,
      reify,
      Info(TyConI),
      Dec(DataD),
      Con(RecC, NormalC),
      Exp(ConE, AppE, VarE),
      Pat(VarP),
      Pat(ConP) )
import Language.Haskell.TH.Desugar
    ( DExp,
      DType(DConT, DAppT),
      dsExp,
      dsType,
      Desugar(sweeten) )
import Data.Maybe (fromMaybe)
import DoNotationDesugar (doNotationAsInScala)
import Language.Haskell.TH (Exp (LamE), Pat (TupP), Quote (newName))
import qualified Haskquill.Qdsl as Q
import Data.Int (Int64)
import Database.PostgreSQL.Simple
    ( query_,
      query,
      execute_,
      execute,
      Only(Only), Connection )
import GHC.Exts (IsString(..))
import Data.Data (Typeable)
import LiftType (liftTypeQ)
import qualified Language.Haskell.TH.Syntax as TS
import Control.Monad (unless)
import Prelude hiding (exp)
import qualified Data.Set as Set
import Data.Time.LocalTime (LocalTime(LocalTime))
import Ast
import SqlQuery
import PrintSql
import RenameProperties
import Parser
import Traversal (mapAst)

dealias :: AST -> (AST, Maybe Name)
dealias (FlatMap q n body) =
  let (FlatMap a b c, _) = dealias' q n body FlatMap in
  let (cn, cnt) = dealias c in
    (FlatMap a b cn, cnt)
dealias (Map q n body) = dealias' q n body Map
dealias (Filter q n body) = dealias' q n body Filter
dealias (GroupBy q n body) = dealias' q n body GroupBy
dealias (OrderBy q n body) = dealias' q n body OrderBy
dealias (Join ty ast1 alias1 ast2 alias2 on) =
  let ((ast1', alias1', on'), _) = dealias' ast1 alias1 on (,,) in
  let ((ast2', alias2', on''), _) = dealias' ast2 alias2 on' (,,) in
    (Join ty ast1' alias1' ast2' alias2' on'', Nothing)
dealias q = (q, Nothing)

dealias' :: AST -> Name -> AST -> (AST -> Name -> AST -> a) -> (a, Maybe Name)
dealias' a b c f =
  case dealias a of
    (an, t@(Just alias)) ->
      (f an alias $ reduction [(b, Ident alias)] c, t)
    _ -> (f a b c, Just b)

expandedTuple :: AST -> (AST, AST)
expandedTuple (Join ty q1@(Join {}) n1 q2@(Join {}) n2 on) =
  let (q1', tup1) = expandedTuple q1 in
  let (q2', tup2) = expandedTuple q2 in
  let on' = reduction [(n1, tup1), (n2, tup2)] on in
    (Join ty q1' n1 q2' n2 on', Tuple [tup1, tup2])
expandedTuple (Join ty q1@(Join {}) n1 q2 n2 on) =
  let (q1', tup1) = expandedTuple q1 in
  let on' = reduction [(n1, tup1)] on in
    (Join ty q1' n1 q2 n2 on', Tuple [tup1, Ident n2])
expandedTuple (Join ty q1 n1 q2@(Join {}) n2 on) =
  let (q2', tup2) = expandedTuple q2 in
  let on' = reduction [(n2, tup2)] on in
    (Join ty q1 n1 q2' n2 on', Tuple [Ident n1, tup2])
expandedTuple (Join ty q1 n1 q2 n2 on) = (Join ty (expandJoin q1) n1 (expandJoin q2) n2 (expandJoin on), Tuple [Ident n1, Ident n2])
expandedTuple _ = error "Incorrect argument: expected join"

isJoin :: AST -> Bool
isJoin (Join {}) = True
isJoin _ = False

expandJoin :: AST ->  AST
expandJoin = mapAst act
  where
    act :: AST -> AST
    act (Filter jq n body) | isJoin jq = go Filter jq n body
    act (Map jq n body) | isJoin jq = go Map jq n body
    act (FlatMap jq n body) | isJoin jq = go FlatMap jq n body
    act (GroupBy jq n body) | isJoin jq = go GroupBy jq n body
    act (OrderBy jq n body) | isJoin jq = go OrderBy jq n body
    act jq@(Join {}) = fst $ expandedTuple jq
    act a = a

    go f jq n body =
      let (jq', tup) = expandedTuple jq in
      f jq' n (expandJoin $ reduction [(n, tup)] body)

queryToSql :: Code Q a -> Q (String, [DExp])
queryToSql q = do
  qexp <- examineCode q
  dQexp <- dsExp $ doNotationAsInScala $ unType qexp
  pAst <- parse [] dQexp
  let vars = freeVars pAst
  unless (Set.null vars) (fail $ "Unexpected free variables: " ++ show vars)
  let ast = reduction [] $ expandJoin $ fst $ dealias pAst
  if isQuery ast then
    let sql = astToSql ast in
    pure (printSqlQuery sql, expandLiftings sql)
  else
    let ast' = renameProps ast in
    pure (printSql ast', expandLiftingsAst ast')

expandLiftings :: SqlQuery -> [DExp]
expandLiftings (SetOperation q1 _ q2) = expandLiftings q1 ++ expandLiftings q2
expandLiftings (Flatten q) =
  let slifts = expandLiftingsAst <$> q.selects in
  let flifts = q.from' >>= expandLiftingsCtx in
  let wlifts = expandLiftingsAst <$> q.where' in
  let glifts = expandLiftingsAst <$> q.group in
  let ordLifts = q.orderBy' >>= (expandLiftingsAst . fst) in
  let lLifts = expandLiftingsAst <$> q.limit in
  let offLifts = expandLiftingsAst <$> q.offset in
  fromMaybe [] $
    slifts <> pure flifts <> wlifts <> glifts
    <> pure ordLifts <> lLifts <> offLifts

expandLiftingsAst :: AST -> [DExp]
expandLiftingsAst (UnaryOperation _ ast) = expandLiftingsAst ast
expandLiftingsAst (Operation op1 _ op2) = expandLiftingsAst op1 ++ expandLiftingsAst op2
expandLiftingsAst (Ident {}) = []
expandLiftingsAst (Property ast _) = expandLiftingsAst ast
expandLiftingsAst (Const {}) = []
expandLiftingsAst (Aggregation _ ast) = expandLiftingsAst ast
expandLiftingsAst (Tuple values) =  values >>= expandLiftingsAst
expandLiftingsAst (Act act) =
  case act of
    (Insert _ assigments) ->
      assigments >>= expandLiftingsAst . snd
    (Update q _ assigments) ->
      (assigments >>= expandLiftingsAst . snd) ++ expandLiftingsAst q
    (Delete q) ->
      expandLiftingsAst q
expandLiftingsAst (Lift exp) = [exp]
expandLiftingsAst q = expandLiftings $ astToSql q

expandLiftingsCtx :: FromContext -> [DExp]
expandLiftingsCtx (TableContext {}) = []
expandLiftingsCtx (QueryContext q _) = expandLiftings q
expandLiftingsCtx (JoinContext ctx1 _ ctx2 on) =
  expandLiftingsCtx ctx1 ++ expandLiftingsCtx ctx2 ++  expandLiftingsAst on

applyArgs :: Exp -> [Exp] -> Exp
applyArgs = foldl AppE

data ReturnType
  = PrimTy TS.Name
  | ConTy TS.Name [ReturnType]
  | MaybeTy ReturnType

transformToCons :: [DType] -> DType -> Q ReturnType
transformToCons args (DAppT fun arg) = transformToCons (arg : args) fun
transformToCons args (DConT con)
  | con == ''Maybe =
    MaybeTy <$> transformToCons [] (head args)
  | otherwise = do
    dataType <- reify con
    case dataType of
      TyConI (DataD _ _ _ _ [RecC conName fields] _) | conName /= 'LocalTime -> do
        args' <- traverse (\(_, _, t) -> dsType t >>= transformToCons []) fields
        pure $ ConTy conName args'
      TyConI (DataD _ _ _ _ [NormalC tup _] []) | ',' `elem` show tup -> do
        args' <- traverse (transformToCons []) args
        pure $ ConTy tup args'
      _ -> PrimTy <$> newName "x"
transformToCons _ _ = PrimTy <$> newName "x"

applicativeApply :: Exp -> [ReturnType] -> Exp
applicativeApply = foldl
      (\exp' arg ->
        case arg of
          MaybeTy arg' -> AppE (AppE (VarE '(<*>)) exp') (AppE (ConE 'Just) (transFunBody arg'))
          _ -> AppE (AppE (VarE '(<*>)) exp') (transFunBody arg))

transFunBody :: ReturnType -> Exp
transFunBody (PrimTy n) = VarE n
transFunBody (ConTy conName args) = applyArgs (ConE conName) (transFunBody <$> args)
transFunBody (MaybeTy (PrimTy n)) = VarE n
transFunBody (MaybeTy (ConTy conName (arg : args))) =
  applicativeApply (AppE (AppE (VarE '(<$>)) (ConE conName)) (transFunBody arg)) args
transFunBody (MaybeTy (ConTy conName [])) = AppE (ConE 'Just) (ConE conName)
transFunBody _ = error "when create function body"

isPrimTy :: ReturnType -> Maybe TS.Name
isPrimTy (PrimTy n) = Just n
isPrimTy (MaybeTy ty) = isPrimTy ty
isPrimTy _ = Nothing

nameList :: ReturnType -> [TS.Name]
nameList (PrimTy n) = [n]
nameList (MaybeTy ty) = nameList ty
nameList (ConTy _ args) = concatMap nameList args

transFun :: ReturnType -> Exp
transFun retTy =
  let body = transFunBody retTy in
  case isPrimTy retTy of
    Just n -> LamE [ConP 'Only [] [VarP n]] (VarE n)
    _ -> LamE [TupP $ VarP <$> nameList retTy] body

{-| Runs a query that returns a set of records.

 @
   $$(runQuery [|| do
      (\t -> t.s) <$> Q.orderBy (\t -> t.s) (Q.from @TestEntity)
    ||])
 @
-}
runQuery :: forall a. Typeable a => Code Q (Q.Query a) -> Code Q (Connection -> IO [a])
runQuery q = liftCode $ do
  (queryString, lifts) <- queryToSql q
  reportWarning queryString
  ty <- liftTypeQ @a >>= dsType
  let transform = transFun <$> transformToCons [] ty
  TExp <$> [| \conn -> do
        result <- $(
          case lifts of
            [] -> [| query_ conn (fromString queryString) |]
            [exp] -> [| query conn (fromString queryString) $(pure $ AppE (ConE 'Only) $ sweeten exp) |]
            _ -> [| query conn (fromString queryString) $(pure $ TS.TupE $ Just . sweeten <$> lifts) |]
          )
        pure ($transform <$> result)
      |]

{-| Runs a query that returns a single value rather than a set of records.

 @
   $$(runSingle [|| 
      Q.max ((\t -> i t) <$> (Q.from @TestEntity))
    ||]) conn
 @
-}
runSingle :: forall a. Typeable a => Code Q a -> Code Q (Connection -> IO a)
runSingle q = liftCode $ do
  (queryString, lifts) <- queryToSql q
  reportWarning queryString
  ty <- liftTypeQ @a >>= dsType
  let transform = transFun <$> transformToCons [] ty
  TExp <$> [| \conn -> do
        result <- $(
          case lifts of
            [] -> [| query_ conn (fromString queryString) |]
            [exp] -> [| query conn (fromString queryString) $(pure $ AppE (ConE 'Only) $ sweeten exp) |]
            _ -> [| query conn (fromString queryString) $(pure $ TS.TupE $ Just . sweeten <$> lifts) |]
          )
        pure (head $ $transform <$> result)
      |]

{-| Runs a sql command (Insert, Update or Delete).

 @
   $$(runAction [|| 
      Q.delete (Q.filter (\a -> a.s == "g") (Q.from @TestEntity))
    ||]) conn
 @
-}
runAction :: Code Q (Q.Action a) -> Code Q (Connection -> IO Int64)
runAction q = liftCode $ do
  (queryString, lifts) <- queryToSql q
  reportWarning queryString
  TExp <$> [| \conn -> $(
          case lifts of
            [] -> [| execute_ conn (fromString queryString) |]
            [exp] -> [| execute conn (fromString queryString) $(pure $ AppE (ConE 'Only) $ sweeten exp) |]
            _ -> [| execute conn (fromString queryString) $(pure $ TS.TupE $ Just . sweeten <$> lifts) |]
        )
      |]


{-| Returns converts the passed query into sql.

 @
   $$(showQuery [|| 
      Q.delete (Q.filter (\a -> a.s == "g") (Q.from @TestEntity))
    ||])
 @
-}
showQuery :: Code Q a -> Code Q String
showQuery q = liftCode $ do
  sql <- fst <$> queryToSql q
  TExp <$> [| sql |]
