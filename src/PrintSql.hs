{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
module PrintSql where

import Ast
import Data.List ( intercalate )
import SqlQuery (astToSql, SqlQuery (..), FromContext (..), FlattenSqlQuery (..))

scopedPrint :: AST -> String
scopedPrint a@(Const {}) = printSql a
scopedPrint a@(Property {}) = printSql a
scopedPrint a@(Ident {}) = printSql a
scopedPrint a@(Lift {}) = printSql a
scopedPrint s = "(" ++ printSql s ++ ")"

showUpdate :: String -> Name -> [(Name, AST)] -> String
showUpdate table alias assigments =
  "UPDATE " ++ table ++ " " ++ show alias ++ " SET " ++
  intercalate ", " ((\(field, val) -> show field ++ " = " ++ printSql val) <$> assigments)

printSql :: AST -> [Char]
printSql (UnaryOperation op ast) = show op ++ " (" ++ printSql ast ++ ")"
printSql (Operation op1 op op2) = scopedPrint op1 ++ " " ++ show op ++ " " ++ scopedPrint op2
printSql (Ident n) = show n
printSql (Property ast prop) = scopedPrint ast ++ "." ++ show prop
printSql (Const c) = show c
printSql (Aggregation aggTy (Ident n)) = show aggTy ++ "(" ++ show n ++ ")"
printSql (Aggregation aggTy (Tuple {})) = show aggTy ++ "(*)"
printSql (Aggregation aggTy ast@(Const {})) = show aggTy ++ "(" ++ printSql ast ++ ")"
printSql (Aggregation aggTy ast@(Operation {})) = show aggTy ++ "(" ++ printSql ast ++ ")"
printSql (Aggregation aggTy ast@(Property {})) = show aggTy ++ "(" ++ printSql ast ++ ")"
printSql (Tuple values) = intercalate ", " (printSql <$> values)
printSql (Act act) =
    case act of
      (Insert (Entity table _) assigments) ->
        "INSERT INTO " ++ table ++ " (" ++ intercalate ", " (show . fst <$> assigments) ++
        ") VALUES (" ++ intercalate ", " (printSql . snd <$> assigments) ++ ")"
      (Update (Entity table _) alias assigments) ->
        showUpdate table alias assigments
      (Update (Filter (Entity table _) n body) alias assigments) ->
        showUpdate table alias assigments ++
        " WHERE " ++ printSql (reduction [(n, Ident alias)] body)
      (Delete (Entity table _)) ->
        "DELETE FROM " ++ table
      (Delete (Filter (Entity table _) n body)) ->
        "DELETE FROM " ++ table ++ " " ++ show n ++
        " WHERE " ++ printSql body
      _ -> error "Incorrect action"
printSql (Lift {}) = "?"
printSql q = printSqlQuery $ astToSql q

printFlattenSql :: FlattenSqlQuery -> [Char]
printFlattenSql q =
  let selectClause =
        case q.selects of
          Nothing -> "SELECT * FROM " ++ intercalate ", " (printContext <$> q.from')
          Just s -> "SELECT " ++ printSql (expandSelectFields s) ++ " FROM " ++ intercalate ", " (printContext <$> q.from') in
  let withWhere =
        case q.where' of
          Nothing -> selectClause
          Just w -> selectClause ++ " WHERE " ++ printSql w in
  let withGroupBy =
        case q.group of
          Nothing -> withWhere
          Just g -> withWhere ++ " GROUP BY " ++ printSql (expandSelectFields g) in
  let withOrderBy =
        case q.orderBy' of
          [] -> withGroupBy
          o -> withGroupBy ++ " ORDER BY " ++ intercalate ", " ((\(ast, prop) -> scopedPrint ast ++ "." ++ show prop) <$> o) in
  case (q.limit, q.offset) of
    (Nothing, Nothing) -> withOrderBy
    (Just l, Nothing) -> withOrderBy ++ " LIMIT " ++ printSql l
    (Just l, Just o) -> withOrderBy ++ " LIMIT " ++ printSql l ++ " OFFSET " ++ printSql o
    (Nothing, Just o) -> withOrderBy ++ " OFFSET " ++ printSql o

expandSelectFields :: AST -> AST
expandSelectFields (Ident n) = Property (Ident n) (NameStr "*")
expandSelectFields (Tuple values) = Tuple $ expandSelectFields <$> values
expandSelectFields a = a

printContext :: FromContext -> [Char]
printContext (TableContext table alias) = table ++ " " ++ show alias
printContext (QueryContext q alias) = "(" ++ printSqlQuery q ++ ") " ++ show alias
printContext (JoinContext ctx1 ty ctx2 on) =
  printContext ctx1 ++ " " ++ show ty ++ " " ++ printContext ctx2 ++ " ON " ++ printSql on

printSqlQuery :: SqlQuery -> [Char]
printSqlQuery (SetOperation q1 op q2) = printSqlQuery q1 ++ " " ++ show op ++ " " ++ printSqlQuery q2
printSqlQuery (Flatten q) = printFlattenSql q
