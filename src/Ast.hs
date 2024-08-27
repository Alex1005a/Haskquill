module Ast where

import qualified Language.Haskell.TH.Syntax as TS
import Language.Haskell.TH.Desugar (DExp)
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

data Name
  = TsName TS.Name
  | TupleIdx Int
  | NameStr String
  deriving (Eq, Ord)

instance Show Name where
  show (TsName n) = TS.nameBase n
  show (TupleIdx i) = show i
  show (NameStr n) = n

data Constant
  = CBool Bool
  | CString String
  | CInt Integer
  | NullValue
  | LocalTimeNow
  deriving Eq

instance Show Constant where
  show (CBool b) = show b
  show (CString s) = "'" ++ s ++ "'"
  show (CInt i) = show i
  show NullValue = "null"
  show LocalTimeNow = "now()"

data Operator
  = Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | GreaterThanOrEq
  | LessThanOrEq
  | ConcatString
  | And
  | Or
  | Add
  | Sub
  | Mul
  | Div
  deriving Eq

data UOperator
  = NonEmpty
  | IsEmpty
  deriving Eq

instance Show UOperator where
  show NonEmpty = "EXISTS"
  show IsEmpty = "NOT EXISTS"

instance Show Operator where
  show Equal = "="
  show NotEqual = "<>"
  show GreaterThan = ">"
  show GreaterThanOrEq = ">="
  show LessThan = "<"
  show LessThanOrEq = "<="
  show ConcatString = "||"
  show And = "AND"
  show Or = "OR"
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Mul = "%"

data AggType
  = Min
  | Max
  | Avg
  | Sum
  | Size
  deriving Eq

instance Show AggType where
  show Max = "MAX"
  show Min = "MIN"
  show Avg = "AVG"
  show Sum = "SUM"
  show Size = "COUNT"

data JoinType = Inner | LeftJoin | RightJoin | FullJoin
  deriving Eq

instance Show JoinType where
  show Inner = "INNER JOIN"
  show LeftJoin = "LEFT JOIN"
  show RightJoin = "RIGHT JOIN"
  show FullJoin = "FULL JOIN"

data ActionOp
  = Insert AST [(Name, AST)] -- only Entity
  | Update AST Name [(Name, AST)] -- Entity or Filter Entity
  | Delete AST -- as well as update
  deriving Eq

data AST
  = Entity String [(Name, String)]
  | Union Bool AST AST
  | Filter AST Name AST
  | Map AST Name AST
  | FlatMap AST Name AST
  | GroupBy AST Name AST
  | OrderBy AST Name AST
  | Join JoinType AST Name AST Name AST
  | Aggregation AggType AST
  | Take AST AST
  | Drop AST AST

  | Tuple [AST]
  | Const Constant

  | Operation AST Operator AST
  | UnaryOperation UOperator AST
  | FunApply AST [AST]

  | Act ActionOp

  | Property AST Name
  | Ident Name
  | Function [Name] AST
  | Lift DExp
  deriving Eq

reductionStep :: [(Name, AST)] -> AST -> AST
reductionStep _ (Property (Tuple values) (TupleIdx pr)) = values !! pr
reductionStep env (FunApply (Function params body) args) =
  if length params == length args then
    reductionStep (zip params args ++ env) body
  else error "The number of arguments passed does not match the number of function parameters"
reductionStep env (Function params body) =
  Function params (reductionStep (filter (\(n', _) -> n' `notElem` params) env) body)
reductionStep env (FunApply fun args) = FunApply (reductionStep env fun) (reductionStep env <$> args)
reductionStep env (Property ast pr) = Property (reductionStep env ast) pr
reductionStep env a@(Ident n) = fromMaybe a $ lookup n env
reductionStep env (FlatMap q n body) =
  FlatMap (reductionStep env q) n (reductionStep (filter (\(n', _) -> n' /= n) env) body)
reductionStep env (Map q n body) =
  Map (reductionStep env q) n (reductionStep (filter (\(n', _) -> n' /= n) env) body)
reductionStep env (Filter q n body) =
  Filter (reductionStep env q) n (reductionStep (filter (\(n', _) -> n' /= n) env) body)
reductionStep env (OrderBy q n body) =
  OrderBy (reductionStep env q) n (reductionStep (filter (\(n', _) -> n' /= n) env) body)
reductionStep env (Join ty ast1 n1 ast2 n2 on) =
  Join ty (reductionStep env ast1) n1 (reductionStep env ast2) n2 (reductionStep (filter (\(n', _) -> n' /= n1 && n' /= n2) env) on)
reductionStep env (GroupBy q n body) =
  GroupBy (reductionStep env q) n (reductionStep (filter (\(n', _) -> n' /= n) env) body)
reductionStep env (Operation op1 op op2) = Operation (reductionStep env op1) op (reductionStep env op2)
reductionStep env (UnaryOperation op ast) = UnaryOperation op (reductionStep env ast)
reductionStep env (Tuple values) = Tuple $ reductionStep env <$> values
reductionStep env (Aggregation agg q) = Aggregation agg $ reductionStep env q
reductionStep env (Take i q) = Take (reductionStep env i) (reductionStep env q)
reductionStep env (Drop i q) = Drop (reductionStep env i) (reductionStep env q)
reductionStep env (Union isAll q1 q2) = Union isAll (reductionStep env q1) (reductionStep env q2)
reductionStep env (Act (Insert table assigments)) = Act (Insert table (second (reductionStep env) <$> assigments))
reductionStep env (Act (Update table n assigments)) = Act (Update table n (second (reductionStep env) <$> assigments))
reductionStep env (Act (Delete q)) = Act $ Delete $ reductionStep env q
reductionStep _ q@(Entity {}) = q
reductionStep _ q@(Const {}) = q
reductionStep _ q@(Lift {}) = q

reduction :: [(Name, AST)] -> AST -> AST
reduction env ast =
  let rast = reductionStep env ast in
    if rast == ast then
      ast
    else reduction [] rast

freeVars :: AST -> Set.Set Name
freeVars (Ident n) = Set.singleton n
freeVars (Union _ q1 q2) = freeVars q1 `Set.union` freeVars q2
freeVars (Filter q n body) = freeVars q `Set.union` Set.delete n (freeVars body)
freeVars (Map q n body) = freeVars q `Set.union` Set.delete n (freeVars body)
freeVars (FlatMap q n body) = freeVars q `Set.union` Set.delete n (freeVars body)
freeVars (GroupBy q n body) = freeVars q `Set.union` Set.delete n (freeVars body)
freeVars (OrderBy q n body) = freeVars q `Set.union` Set.delete n (freeVars body)
freeVars (Join _ q1 n1 q2 n2 on) =
  freeVars q1 `Set.union` freeVars q2 `Set.union`
    Set.difference (freeVars on) (Set.fromList [n1, n2])
freeVars (Aggregation _ q) = freeVars q
freeVars (Take i q) = freeVars i `Set.union` freeVars q
freeVars (Drop i q) = freeVars i `Set.union` freeVars q
freeVars (Tuple values) = foldl Set.union Set.empty $ freeVars <$> values
freeVars (Operation q1 _ q2) = freeVars q1 `Set.union` freeVars q2
freeVars (UnaryOperation _ q) = freeVars q
freeVars (Act (Insert _ assigments)) = foldl Set.union Set.empty $ freeVars . snd <$> assigments
freeVars (Act (Update q n assigments)) =
  freeVars q `Set.union` Set.delete n (foldl Set.union Set.empty $ freeVars . snd <$> assigments)
freeVars (Act (Delete q)) = freeVars q
freeVars (Function params body) = Set.difference (freeVars body) (Set.fromList params)
freeVars (FunApply fun args) = freeVars fun `Set.union` foldl Set.union Set.empty (freeVars <$> args)
freeVars (Property q _) = freeVars q
freeVars (Const {}) = Set.empty
freeVars (Entity {}) = Set.empty
freeVars (Lift {}) = Set.empty

isQuery :: AST -> Bool
isQuery (Entity {}) = True
isQuery (Filter {}) = True
isQuery (Map {}) = True
isQuery (FlatMap {}) = True
isQuery (OrderBy {}) = True
isQuery (GroupBy {}) = True
isQuery (Aggregation {}) = True
isQuery (Take {}) = True
isQuery (Drop {}) = True
isQuery (Union {}) = True
isQuery (Join {}) = True
isQuery _ = False
