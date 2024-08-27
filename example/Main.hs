{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where
import Database.PostgreSQL.Simple
import Control.Monad (void)
import Data.Text hiding (head)
import Haskquill ( runQuery, runAction )
import qualified Haskquill.Qdsl as Q
import Data.Foldable (traverse_)
import Prepare

{-
To build it use command: stack build --flag haskquill:build-example.

This module is based on a repository https://github.com/Zelenya/elephants
that shows examples of working with postgresql using various haskell libraries.
-}

connInfo :: ConnectInfo
connInfo =
    defaultConnectInfo
      { connectHost = "127.0.0.1"
      , connectDatabase = "warehouse"
      , connectUser = "postgres"
      , connectPassword = "1012"
      }

cleanUp :: Connection -> IO ()
cleanUp connection =
  void $ execute_ connection "truncate warehouse, product_category, product, category"

main :: IO ()
main = do
  conn <- connect connInfo
  cleanUp conn

  insertStuff conn
  queryData conn
  insertWithTransaction conn
  queryWithJoins conn  

  close conn

insertProductDyn :: Connection -> Text -> Maybe Text -> IO ()
insertProductDyn conn l d = void $ $$(runAction [||
        Q.insert (Q.from @Product)
            (\p -> [p.label Q.:= Q.lift l, p.description Q.:= Q.lift d])
    ||]) conn

insertStuff :: Connection -> IO ()
insertStuff conn = do
  -- use compile time function
  void $ $$(runAction [||
      $$insertProduct "Wood Screw Kit 1" (Just "245-pieces")
    ||]) conn
  
  void $ $$(runAction [||
      Q.insert (Q.from @Product)
        (\p -> [p.label Q.:= "Wood Screw Kit 2"])
    ||]) conn

  -- or insert using sql parameters
  insertProductDyn conn "Wood Screw Kit 3" Nothing
  insertProductDyn conn "Wood Screw Kit 4" (Just "245-pieces")

  traverse_ (\l ->
        $$(runAction [||
          Q.insert (Q.from @Category)
            (\p -> [p.label Q.:= Q.lift l])
        ||]) conn
      )
    ["Screws", "Wood Screws", "Concrete Screws"]

queryData :: Connection -> IO ()
queryData conn = do
  products <- $$(runQuery [|| Q.from @Product ||]) conn
  putStrLn $ "Query 1: " ++ show products

  productsWithLabel <- $$(runQuery [||
      Q.filter (\p -> p.label == "Wood Screw Kit 2") (Q.from @Product)
    ||]) conn
  putStrLn $ "Query 2: " ++ show productsWithLabel
  -- Query 3: in operator not implemented

insertWithTransaction :: Connection -> IO ()
insertWithTransaction conn = withTransaction conn $ do
  void $ $$(runAction [||
      $$insertProduct "Drywall Screws Set" (Just "8000pcs")
    ||]) conn

  -- there is no returning clause yet
  productId <- head <$> $$(runQuery [||
      (\p -> p.id) <$>
      Q.filter (\p -> p.label == "Drywall Screws Set"
          && p.description == Just "8000pcs")
        (Q.from @Product)
    ||]) conn

  void $ $$(runAction [||
          Q.insert (Q.from @Category)
            (\p -> [p.label Q.:= "Drywall Screws"])
        ||]) conn

  -- there is no returning clause yet
  categoryId <- head <$> $$(runQuery [||
      (\p -> p.id) <$>
      Q.filter (\p -> p.label == "Drywall Screws")
        (Q.from @Category)
    ||]) conn

  void $ $$(runAction [||
      Q.insert $$fromWarehouses (\w -> [
          w.productId Q.:= Q.lift productId,
          w.quantity Q.:= 10,
          w.created Q.:= Just Q.now,
          w.modified Q.:= Just Q.now
        ])
    ||]) conn

  void $ $$(runAction [||
      Q.insert $$fromProductCategories (\pc -> [pc.productId Q.:= Q.lift productId, pc.categoryId Q.:= Q.lift categoryId])
    ||]) conn

queryWithJoins :: Connection -> IO ()
queryWithJoins conn = do
  q <- $$(runQuery [||
      (\(((w, p), pc), c) -> (w.quantity, p.label, p.description, Q.maybeMap (\c -> c.label) c))
      <$> Q.leftJoin (
        Q.leftJoin (
            Q.join $$fromWarehouses (Q.from @Product) (\w p -> w.productId == p.id)
          )
          $$fromProductCategories
          (\(w, p) pc -> p.id == pc.productId)
      ) (Q.from @Category)
      (\((w, p), pc) c -> Q.exists pc (\pc -> c.id == pc.categoryId))
    ||]) conn

  putStrLn $ "Query with join: " ++ show q
