{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}


{-# LANGUAGE ScopedTypeVariables #-}
module TestQsql where

import Haskquill ( runQuery, runSingle, runAction )
import qualified Haskquill.Qdsl as Q
import Domain

schemaTest = $$(runQuery [||
    (\a -> a.s) <$> (Q.fromSchema @TestEntity "ent" (\l -> [l.s Q.:> "ssg"]))
  ||])

delTest = $$(runAction [|| Q.delete (Q.filter (\a -> a.s == "g") (Q.from @TestEntity)) ||])

updateTest = $$(runAction [|| 
    Q.update (Q.filter (\a -> a.s == "g") (Q.from @TestEntity)) (\t -> [t.s Q.:= t.s, t.o Q.:= Just 1])
  ||])

insertTest = $$(runAction [|| Q.insert (Q.from @TestEntity) (\t -> [t.s Q.:= "d", t.i Q.:= 1]) ||])

joinTest = $$(runQuery [|| do
    (w, gol) <- Q.leftJoin (Q.from @TestEntity) (Q.from @TestEntity2) (\z v -> True)
    pure (Q.exists gol (\b -> b.s == Q.lift "(1 :: Int)"))
  ||])

joinQ = $$(runQuery [|| do
    a <- Q.from @TestEntity
    b <- Q.filter (\b1 -> i1 b1 > i a) (Q.from @TestEntity2)
    pure (i a, i1 b)
  ||])

sortQ = $$(runQuery [|| do
    (\t -> t.s) <$> Q.orderBy (\t -> t.s) (Q.from @TestEntity)
  ||])

withFilterQ = $$(runQuery [|| do
    (\t -> i t) <$> Q.orderBy (\t -> t.s) (Q.filter (\t -> t.s == "s") (Q.from @TestEntity))
  ||])

-- reverse

outerFilterQ = $$(runQuery [|| do
    (\t -> t.s) <$> Q.filter (\t -> t.s == "s") (Q.orderBy (\t -> t.s) (Q.fromSchema @TestEntity "ent" (\l -> [l.s Q.:> "ssg"])))
  ||])

withFlatMapQ = $$(runQuery [|| do
    (Q.orderBy (\t -> t.s) (Q.from @TestEntity)) >>= (\t1 -> (\t -> t.s) <$> (Q.from @TestEntity2))
  ||])

tupleCriteriaQ = $$(runQuery [|| do
    (\t -> t.s) <$> Q.orderBy (\t -> (t.s, i t)) (Q.from @TestEntity)
  ||])

-- multiple sortBy (reverse)

groupedQ = $$(runQuery [|| do
    (\(g, t) -> (g, Q.size t)) <$> Q.groupBy (\t -> i t) (Q.from @TestEntity)
  ||])

nestedQ = $$(runQuery [|| do
      (\(g, v) -> g) <$> Q.groupBy (\t -> i t) (Q.from @TestEntity) >>= (\t1 -> Q.from @TestEntity2)
  ||])

tupleQ = $$(runQuery [|| do
      (\(g, v) -> g) <$> Q.groupBy (\t -> (i t, l t)) (Q.from @TestEntity)
  ||])

aggregatedQ = $$(runSingle [|| 
    Q.max ((\t -> i t) <$> (Q.from @TestEntity))
  ||])

limNestedQ = $$(runQuery [|| 
      Q.take 10 (Q.from @TestEntity) >>= (\a -> Q.from @TestEntity2)
  ||])

limWithMapQ = $$(runQuery [|| 
      fmap (\t -> t.s) (Q.take 10 (Q.from @TestEntity))
  ||])

multipleLimsQ = $$(runQuery [|| 
      Q.take 10 (Q.take 1 (Q.from @TestEntity))
  ||])

offsetQ = $$(runQuery [|| 
      Q.skip 10 (Q.from @TestEntity)
  ||])

offsetNestedQ = $$(runQuery [|| 
      Q.skip 10 (Q.from @TestEntity) >>= (\a -> Q.from @TestEntity2)
  ||])

offsetWithMapQ = $$(runQuery [|| 
      fmap (\t -> t.s) (Q.skip 10 (Q.from @TestEntity))
  ||])

multipleOffsetsQ = $$(runQuery [|| 
      Q.skip 10 (Q.skip 1 (Q.from @TestEntity))
  ||])

limAndOffsetQ = $$(runQuery [|| 
      Q.take 11 (Q.skip 10 (Q.from @TestEntity))
  ||])

nestedLimAndOffsetQ = $$(runQuery [|| 
      Q.take 11 (Q.skip 10 (Q.from @TestEntity)) >>= (\a -> Q.from @TestEntity2)
  ||])

multiplieLimAndOffsetQ = $$(runQuery [|| 
      Q.take 4 (Q.skip 3 (Q.take 2 (Q.skip 1 (Q.from @TestEntity))))
  ||])

takedropQ = $$(runQuery [|| 
      Q.skip 2 (Q.take 1 (Q.from @TestEntity))
  ||])

unionQ = $$(runQuery [|| 
      (Q.from @TestEntity) `Q.union` (Q.from @TestEntity)
  ||])

unionAllQ = $$(runQuery [|| 
      (Q.from @TestEntity) `Q.unionAll` (Q.from @TestEntity)
  ||])

