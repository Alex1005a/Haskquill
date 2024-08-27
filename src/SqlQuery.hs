module SqlQuery where

import Ast
import RenameProperties
import Data.Maybe (isNothing)

data FromContext
  = TableContext String Name
  | QueryContext SqlQuery Name
  | JoinContext FromContext JoinType FromContext AST

data SqlSetOp = UnionOp | UnionAllOp

instance Show SqlSetOp where
  show UnionOp = "UNION"
  show UnionAllOp = "UNION ALL"

data SqlQuery
  = SetOperation SqlQuery SqlSetOp SqlQuery
  | Flatten FlattenSqlQuery

data FlattenSqlQuery = FlattenSqlQuery
  {
    from' :: [FromContext],
    where' :: Maybe AST,
    selects :: Maybe AST,
    orderBy' :: [(AST, Name)],
    limit :: Maybe AST,
    offset :: Maybe AST,
    group :: Maybe AST
  }

sqlQuery :: [FromContext] -> FlattenSqlQuery
sqlQuery ctxs = FlattenSqlQuery ctxs Nothing Nothing [] Nothing Nothing Nothing

astToSql :: AST -> SqlQuery
astToSql q = go $ renameProps q
  where
  go (Union False q1 q2) = SetOperation (astToSql q1) UnionOp (astToSql q2)
  go (Union True q1 q2) = SetOperation (astToSql q1) UnionAllOp (astToSql q2)
  go (Act {}) = error "Query not properly normalized"
  go (UnaryOperation {}) = error "Query not properly normalized"
  go (Operation {}) = error "Query not properly normalized"
  go (Ident {}) = error "Query not properly normalized"
  go (Property {}) = error "Query not properly normalized"
  go (Const {}) = error "Query not properly normalized"
  go (Tuple {}) = error "Query not properly normalized"
  go (Function {}) = error "Query not properly normalized"
  go (FunApply {}) = error "Query not properly normalized"
  go (Lift {}) = error "Query not properly normalized"
  go ast =
    let (contexts, body) = flattenContexts ast in
      Flatten $ flatten (NameStr "x") contexts body

aliases :: FromContext -> [Name]
aliases (TableContext _ alias) = [alias]
aliases (QueryContext _ alias) = [alias]
aliases (JoinContext ctx1 _ ctx2 _) = aliases ctx1 ++ aliases ctx2

base :: [FromContext] -> AST -> Name -> FlattenSqlQuery
base _ q@(Join {}) alias =
  let ctx = source q alias in
    (sqlQuery [ctx]) { selects = Just $ Tuple (Ident <$> aliases ctx) }
base contexts q@(Map {}) alias = flatten alias contexts q
base contexts q@(Filter {}) alias = flatten alias contexts q
base contexts q@(Entity {}) alias = flatten alias contexts q
base [] q alias = flatten alias [] q
base contexts other alias = (sqlQuery $ source other alias : contexts) { selects = Just $ Ident alias }

source :: AST -> Name -> FromContext
source (Entity table _) name = TableContext table name
source (Join ty ast1 alias1 ast2 alias2 on) _ =
  JoinContext (source ast1 alias1) ty (source ast2 alias2) on
source other name = QueryContext (astToSql other) name

flattenContexts :: AST -> ([FromContext], AST)
flattenContexts (FlatMap q n body) =
  let ctx = source q n
      (contexts, body') = flattenContexts body in
      (ctx : contexts, body')
flattenContexts other = ([], other)

flatten :: Name -> [FromContext] -> AST -> FlattenSqlQuery
flatten _ _ (GroupBy {}) = error "groupBy clause must be followed by fmap."
flatten _ contexts (Map (GroupBy q n body) nm mbody) =
  let b = base contexts q nm in
  let select = reduction [(nm, Tuple [body, Ident n])] mbody in
    b { group = Just body, selects = Just select }
flatten _ contexts (Map q n body) =
  let b = base contexts q n in
    case selects b of
      Just (Aggregation {}) -> (sqlQuery [QueryContext (astToSql q) n]) { selects = Just body }
      Just selectAst ->  b { selects = Just $ reduction [(n, selectAst)] body }
      Nothing -> b { selects = Just body }
flatten _ contexts (Filter q n body) =
  let b = base contexts q n in
    if isNothing $ where' b then
      b { where' = Just body }
    else
      (sqlQuery [QueryContext (astToSql q) n]) { where' = Just body, selects = Just $ Ident n }
flatten _ contexts (OrderBy q n body) =
  let b = base contexts q n in
    if null $ orderBy' b then
      b { orderBy' = orderByCriterias body }
    else
      (sqlQuery [QueryContext (astToSql q) n]) { orderBy' = orderByCriterias body, selects = Just $ Ident n }
flatten alias contexts (Aggregation agg q) =
  let b = flatten alias contexts q in
    case selects b of
      Just s -> b { selects = Just $ Aggregation agg s }
      Nothing ->
        b {
          from' = [QueryContext (astToSql q) alias],
          selects = Just $ Aggregation agg (Ident $ NameStr "*")
        }
flatten alias contexts (Take n q) =
   let b = base contexts q alias in
    if isNothing $ limit b then
      b { limit = Just n }
    else
      (sqlQuery [QueryContext (astToSql q) alias]) { limit = Just n, selects = Just $ Ident alias }
flatten alias contexts (Drop n q) =
  let b = base contexts q alias in
    if isNothing (limit b) && isNothing (offset b) then
      b { offset = Just n }
    else
      (sqlQuery [QueryContext (astToSql q) alias]) { offset = Just n, selects = Just $ Ident alias }
flatten alias contexts other = (sqlQuery (source other alias : contexts)) { selects = Just $ Ident alias }

orderByCriterias :: AST -> [(AST, Name)]
orderByCriterias (Property ast prop) = [(ast, prop)]
orderByCriterias (Tuple args) = args >>= orderByCriterias
orderByCriterias _ = error "Invalid order by criteria"
