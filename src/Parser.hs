{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Parser where

import Language.Haskell.TH.Desugar
import Ast
import qualified Language.Haskell.TH.Syntax as TS
import qualified Haskquill.Qdsl as Q
import GHC.Records (getField)
import Language.Haskell.TH
    ( Q,
      Con(RecC),
      Type(ConT, AppT, ArrowT),
      Dec(DataD),
      nameBase,
      reify,
      Info(TyConI, VarI),
      Lit(IntegerL, StringL),
      TyLit(StrTyLit) )
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf, unfoldr)
import GHC.Base (liftA2)
import Data.Bifunctor (second)
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Prelude hiding (exp)

split :: Eq a => a -> [a] -> [[a]]
split x = unfoldr (fmap coalg) . Just
 where
  coalg t = (f, n)
   where
    (f, dfs) = break (x ==) t
    n = case dfs of
      [] -> Nothing
      (_ : fs) -> Just fs

nameToOp :: Map.Map TS.Name Operator
nameToOp = Map.fromList [
    ('(==), Equal),
    ('(/=), NotEqual),
    ('(>), GreaterThan),
    ('(>=), GreaterThanOrEq),
    ('(<), LessThan),
    ('(++), ConcatString),
    ('(<=), LessThanOrEq),
    ('(&&), And),
    ('(||), Or),
    ('(+), Add),
    ('(-), Sub),
    ('(*), Mul),
    ('(/), Div)
  ]

nameToJoinTy :: Map.Map TS.Name JoinType
nameToJoinTy = Map.fromList [
    ('Q.join, Inner),
    ('Q.leftJoin, LeftJoin),
    ('Q.rightJoin, RightJoin),
    ('Q.fullJoin, FullJoin)
  ]

property :: AST -> [Int] -> AST
property = foldl (\ast i -> Property ast (TupleIdx i))

parsePattern :: AST -> [Int] -> DPat -> [(Name, AST)]
parsePattern tup path (DVarP name) = [(TsName name, property tup path)]
parsePattern tup path (DConP con _ values) | ',' `elem` show con =
  zip values [0..] >>= (\(val, idx) -> parsePattern tup (path ++ [idx]) val)
parsePattern _ _ _ = error "Incorrect pattern matching"

parseList :: DExp -> [DExp]
parseList (DAppE (DAppE (DConE conName) e) tailList) | conName == '(:) = e : parseList tailList
parseList (DConE conName) | conName == '[] = []
parseList _ = error "Expression not list"

parseCon :: TS.Name -> TS.Name -> DExp -> Q [(Name, AST)]
parseCon con' alias assigmentsExp = do
  qAssigments <- traverse (\case
      (DAppE (DAppE (DConE con) e) valExp) | con == con' -> liftA2 (,) (parse [] e) (parse [] valExp)
      _ -> error "Invalid constructor"
    ) $ parseList assigmentsExp
  pure $ (\case
      (Property (Ident (TsName a)) n, valAst) | a == alias -> (n, valAst)
      _ -> error $ "Invalid property when constructor " ++ show con' ++ " parsed"
    ) <$> qAssigments

findAlias :: Name -> DExp -> Q (Maybe Name)
findAlias n (DCaseE sc [DMatch pat _]) = do
  env <- (\sc' -> parsePattern sc' [] pat) <$> parse [] sc
  pure $ fst <$> find (\(_, val) ->
          case reduction [] val of
            Ident n' | n == n' -> True
            _ -> False
        ) env
findAlias _ _ = pure Nothing

parse :: [DExp] -> DExp -> Q AST
parse args (DAppE fun arg) = parse (arg : args) fun
parse [] (DAppTypeE (DVarE n) (DConT ty)) | n == 'Q.from = pure $ Entity (toLower <$> nameBase ty) []
parse [DLitE (StringL table), DLamE [alias] body] (DAppTypeE (DVarE n) (DConT _)) | n == 'Q.fromSchema = do
  propAlias <- parseCon '(Q.:>) alias body
  pure $ Entity table (second (\case
    (Const (CString s)) -> s
    _ -> error "Propery alias must be string alias") <$> propAlias)
parse args (DConE n) | ',' `elem` show n && "GHC.Tuple." `isPrefixOf` show n =
  if length (filter (== ',') (show n)) + 1 == length args then do
    pargs <- traverse (parse []) args
    pure $ Tuple pargs
  else error "Incorrect tuple"
parse [] (DConE n) | n == 'True = pure $ Const $ CBool True
parse [] (DConE n) | n == 'False = pure $ Const $ CBool False
parse [] (DVarE n) | n == 'Q.now = pure $ Const LocalTimeNow
parse [] (DLitE (StringL s)) = pure $ Const $ CString s
parse [] (DLitE (IntegerL i)) = pure $ Const $ CInt i
parse [] (DCaseE sc [DMatch pat caseBody]) = do
  env <- (\sc' -> parsePattern sc' [] pat) <$> parse [] sc
  let env' = filter (\(_, val) ->
          case reduction [] val of
            Ident {} -> False
            _ -> True
        ) env
  pbody <- parse [] caseBody
  pure $ reduction env' pbody
parse [nexp, qexp] (DVarE n) | n == 'Q.take = do
  nast <- parse [] nexp
  qast <- parse [] qexp
  pure $ Take nast qast
parse [nexp, qexp] (DVarE n) | n == 'Q.skip = do
  nast <- parse [] nexp
  qast <- parse [] qexp
  pure $ Drop nast qast
parse [qexp1, qexp2] (DVarE n) | n == 'Q.union = do
  qast1 <- parse [] qexp1
  qast2 <- parse [] qexp2
  pure $ Union False qast1 qast2
parse [qexp1, qexp2] (DVarE n) | n == 'Q.unionAll = do
  qast1 <- parse [] qexp1
  qast2 <- parse [] qexp2
  pure $ Union True qast1 qast2
parse [qexp1, qexp2, DLamE [var1, var2] body] (DVarE n) | Map.member n nameToJoinTy = do
  ast1 <- parse [] qexp1
  ast2 <- parse [] qexp2
  on <- parse [] body
  var1' <- fromMaybe (TsName var1) <$> findAlias (TsName var1) body
  var2' <- fromMaybe (TsName var2) <$> findAlias (TsName var2) body
  pure $ Join (nameToJoinTy Map.! n) ast1 var1' ast2 var2' on
parse [DLamE [var] body, qexp] (DVarE n) | n == 'Q.filter = do
  qAst <- parse [] qexp
  bodyAst <- parse [] body
  pure $ Filter qAst (TsName var) bodyAst
parse [DLamE [var] body, qexp] (DVarE n) | n == 'Q.orderBy = do
  qAst <- parse [] qexp
  bodyAst <- parse [] body
  pure $ OrderBy qAst (TsName var) bodyAst
parse [DLamE [var] body, qexp] (DVarE n) | n == 'Q.groupBy = do
  qAst <- parse [] qexp
  bodyAst <- parse [] body
  pure $ GroupBy qAst (TsName var) bodyAst
parse [DLamE [var] body, qexp] (DVarE n)
      | n == 'fmap
      || n == '(<$>) = do
  qAst <- parse [] qexp
  bodyAst <- parse [] body
  pure $ Map qAst (TsName var) bodyAst
parse [qexp, DLamE [var] body] (DVarE n) | n == '(>>=) = do
  qAst <- parse [] qexp
  bodyAst <- parse [] body
  pure $ FlatMap qAst (TsName var) bodyAst
parse [qexp, DLamE [var] body] (DVarE n) | n == 'Q.exists = do
  qAst <- parse [] qexp
  bodyAst <- parse [] body
  pure $ reduction [(TsName var, qAst)] bodyAst
parse [DLamE [var] body, qexp] (DVarE n) | n == 'Q.maybeMap = do
  qAst <- parse [] qexp
  bodyAst <- parse [] body
  pure $ reduction [(TsName var, qAst)] bodyAst
parse [qexp, DLamE [alias] assigmentsExp] (DVarE n) | n == 'Q.insert = do
  entity <- parse [] qexp
  assigments <- parseCon '(Q.:=) alias assigmentsExp
  pure $ Act (Insert entity assigments)
parse [qexp, DLamE [alias] assigmentsExp] (DVarE n) | n == 'Q.update = do
  ast <- parse [] qexp
  assigments <- parseCon '(Q.:=) alias assigmentsExp
  pure $ Act (Update ast (TsName alias) assigments)
parse [qexp] (DVarE n) | n == 'Q.delete = do
  ast <- parse [] qexp
  pure $ Act (Delete ast)
parse [] (DConE n) | n == 'Nothing = pure $ Const NullValue
parse [qexp] (DConE n) | n == 'Just = parse [] qexp
parse [op1, op2] (DVarE n) | Map.member n nameToOp = do
  astOp1 <- parse [] op1
  astOp2 <- parse [] op2
  let op = nameToOp Map.! n
  pure $ Operation astOp1 op astOp2
parse [qexp] (DVarE n) | n == 'Q.min = do
  qAst <- parse [] qexp
  pure $ Aggregation Min qAst
parse [qexp] (DVarE n) | n == 'Q.max = do
  qAst <- parse [] qexp
  pure $ Aggregation Max qAst
parse [qexp] (DVarE n) | n == 'Q.avg = do
  qAst <- parse [] qexp
  pure $ Aggregation Avg qAst
parse [qexp] (DVarE n) | n == 'Q.sum = do
  qAst <- parse [] qexp
  pure $ Aggregation Sum qAst
parse [qexp] (DVarE n) | n == 'Q.size = do
  qAst <- parse [] qexp
  pure $ Aggregation Size qAst
parse [qexp] (DVarE n) | n == 'Q.nonEmpty = do
  qAst <- parse [] qexp
  pure $ UnaryOperation NonEmpty qAst
parse [qexp] (DVarE n) | n == 'Q.isEmpty = do
  qAst <- parse [] qexp
  pure $ UnaryOperation IsEmpty qAst
parse [exp] (DVarE n) | n == 'Q.lift = pure $ Lift exp
--OverloadedRecordDot
parse [record] (DAppTypeE (DVarE n) (DLitT (StrTyLit field))) | n == 'getField = do
  recordAst <- parse [] record
  pure $ Property recordAst $ TsName $ TS.mkName field
parse [record] (DVarE n) = do
  recordAst <- parse [] record
  appNameInfo <- reify n
  case appNameInfo of
    (VarI _ (AppT (AppT ArrowT (ConT con)) _) _) -> do
      conNameInfo <- reify con
      case conNameInfo of
        (TyConI (DataD _ _ _ _ [RecC conName fields] _)) ->
          -- DuplicateRecordFields
          if isPrefixOf "$sel" (nameBase n) && isSuffixOf (nameBase conName) (nameBase n) then
            pure $ Property recordAst $ NameStr (split ':' (nameBase n) !! 1)
          else
          if any (\(fieldName, _, _) -> fieldName == n) fields then
            pure $ Property recordAst $ TsName n
          else error $ show conNameInfo
        _ -> error $ show conNameInfo
    _ -> error $ show appNameInfo
parse [] (DVarE n) = pure $ Ident $ TsName n
parse [] (DLamE params body) = do
  b <- parse [] body
  pure $ Function (TsName <$> params) b
parse args@(_ : _) fun = do
  astFun <- parse [] fun
  astArgs <- traverse (parse []) args
  pure $ FunApply astFun astArgs
parse _ d = error $ show d
