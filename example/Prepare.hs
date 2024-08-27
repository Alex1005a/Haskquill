{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Prepare where

import Data.Text (Text)
import qualified Haskquill.Qdsl as Q
import Language.Haskell.TH.Syntax (Code, Q)
import Data.Time.LocalTime (LocalTime)

data Product = Product {id :: Int, label :: Text, description :: Maybe Text}
  deriving Show

data Category = Category {id :: Int, label :: Text}
  deriving Show

data Warehouse = Warehouse {id :: Int, productId :: Int, quantity :: Int, created :: Maybe LocalTime, modified :: Maybe LocalTime}
  deriving Show

data ProductCategory = ProductCategory {categoryId :: Int, productId :: Int}
  deriving Show

insertProduct :: Code Q (Text -> Maybe Text -> Q.Action Product)
insertProduct = [|| \l d ->
        Q.insert (Q.from @Product)
            (\p -> [p.label Q.:= l, p.description Q.:= d])
    ||]

fromWarehouses :: Code Q (Q.Query Warehouse)
fromWarehouses = [|| 
        Q.fromSchema @Warehouse "warehouse" (\w -> [w.productId Q.:> "product_id"])
    ||]

fromProductCategories :: Code Q (Q.Query ProductCategory)
fromProductCategories = [|| 
        Q.fromSchema @ProductCategory "product_category" (\w -> [
                w.categoryId Q.:> "category_id",
                w.productId Q.:> "product_id"
            ])
    ||]
