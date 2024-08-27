{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
import Test.HUnit
import Haskquill (showQuery)
import qualified Haskquill.Qdsl as Q
import Domain
import qualified System.Exit as Exit

schemaTest = assertEqual "test fromSchema" "SELECT a.ssg FROM ent a" $$(showQuery [||
    (\a -> a.s) <$> (Q.fromSchema @TestEntity "ent" (\l -> [l.s Q.:> "ssg"]))
  ||])

deleteTest = assertEqual "test delete" "DELETE FROM testentity a WHERE a.s = 'g'"
    $$(showQuery [|| Q.delete (Q.filter (\a -> a.s == "g") (Q.from @TestEntity)) ||])

updateTest = assertEqual "test update" "UPDATE testentity t SET s = t.s, o = 1 WHERE t.s = 'g'"
    $$(showQuery [||
        Q.update (Q.filter (\a -> a.s == "g") (Q.from @TestEntity)) (\t -> [t.s Q.:= t.s, t.o Q.:= Just 1])
    ||])

insertTest = assertEqual "test update" "INSERT INTO testentity (s, i) VALUES ('d', 1)"
    $$(showQuery [||
        Q.insert (Q.from @TestEntity) (\t -> [t.s Q.:= "d", t.i Q.:= 1])
    ||])

joinTest = assertEqual "test join" "SELECT v.s = ? FROM testentity z LEFT JOIN testentity2 v ON True"
    $$(showQuery [|| do
        (w, gol) <- Q.leftJoin (Q.from @TestEntity) (Q.from @TestEntity2) (\z v -> True)
        pure (Q.exists gol (\b -> b.s == Q.lift "(1 :: Int)"))
    ||])

cartesianJoinTest = assertEqual "cartesian test join" "SELECT a.i, b1.i1 FROM testentity2 b1, testentity a WHERE b1.i1 > a.i"
    $$(showQuery [|| do
        a <- Q.from @TestEntity
        b <- Q.filter (\b1 -> i1 b1 > i a) (Q.from @TestEntity2)
        pure (i a, i1 b)
    ||])

sortTest = assertEqual "test sort" "SELECT t.s FROM testentity t ORDER BY t.s"
    $$(showQuery [|| (\t -> t.s) <$> Q.orderBy (\t -> t.s) (Q.from @TestEntity) ||])

withFilterTest = assertEqual "test sort and where" "SELECT t.i FROM testentity t WHERE t.s = 's' ORDER BY t.s"
    $$(showQuery [|| (\t -> i t) <$> Q.orderBy (\t -> t.s) (Q.filter (\t -> t.s == "s") (Q.from @TestEntity)) ||])

orderTupleCriteriaTest = assertEqual "test order tuple criteria" "SELECT t.s FROM testentity t ORDER BY t.s, t.i"
    $$(showQuery [|| (\t -> t.s) <$> Q.orderBy (\t -> (t.s, i t)) (Q.from @TestEntity) ||])

groupByTest = assertEqual "test order tuple criteria" "SELECT t.i FROM testentity t GROUP BY t.i"
    $$(showQuery [|| (\(g, v) -> g) <$> Q.groupBy (\t -> i t) (Q.from @TestEntity) ||])

limitWithMapTest = assertEqual "test limit with select" "SELECT t.s FROM testentity t LIMIT 10"
    $$(showQuery [|| fmap (\t -> t.s) (Q.take 10 (Q.from @TestEntity)) ||])

selectTupleGroupByTest = assertEqual "test select tuple which was used for grouping" "SELECT t.i, t.l FROM testentity t GROUP BY t.i, t.l"
    $$(showQuery [|| (\(g, v) -> g) <$> Q.groupBy (\t -> (i t, l t)) (Q.from @TestEntity) ||])


tests :: Test
tests = TestList $ TestCase <$> [
        schemaTest,
        deleteTest,
        updateTest,
        insertTest,
        joinTest,
        cartesianJoinTest,
        sortTest,
        withFilterTest,
        orderTupleCriteriaTest,
        groupByTest,
        limitWithMapTest,
        selectTupleGroupByTest
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
