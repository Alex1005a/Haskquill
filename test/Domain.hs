{-# LANGUAGE DuplicateRecordFields #-}
module Domain where
import Data.Time.LocalTime (LocalTime)

data TestEntity
  = TestEntity {s :: String, i :: Int, l :: Integer, o :: Maybe Int}
data TestEntity2
  = TestEntity2 {s :: String, i1 :: Int, l1 :: Integer, o1 :: Maybe Int}
data TestEntity3
  = TestEntity3 {s2 :: String, i2 :: Int, l2 :: Integer, o2 :: Maybe Int}

