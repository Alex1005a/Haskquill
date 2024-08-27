{-| This module provides functions that can be used to construct a query.
    Note that __these functions can only be used at compile time, i.e. inside template haskell quotes.__

-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Haskquill.Qdsl (
    Query,
    from,
    fromSchema,
    Haskquill.Qdsl.filter,
    orderBy,
    groupBy,
    join,
    leftJoin,
    rightJoin,
    fullJoin,
    Haskquill.Qdsl.min,
    Haskquill.Qdsl.max,
    Haskquill.Qdsl.avg,
    Haskquill.Qdsl.sum,
    Haskquill.Qdsl.size,
    Haskquill.Qdsl.nonEmpty,
    Haskquill.Qdsl.isEmpty,
    skip,
    Haskquill.Qdsl.take,
    union,
    unionAll,
    exists,
    Action,
    Assigment(..),
    PropertyAlias(..),
    insert,
    update,
    delete,
    Haskquill.Qdsl.lift,
    now,
    maybeMap
) where
import Data.Time (LocalTime)

onlyCompileTime :: a
onlyCompileTime = error "Can not used during runtime"

-- | Recordset of type a
data Query a

{-| Function that allows you to specify the type of records that will be retrieved from the table.
    The table name will be the type name converted to lower case.
    It is assumed that the type passed will be a record with one constructor.
 @
   [|| Q.from @TestEntity ||]
 @
-}
from :: forall a. Query a
from = onlyCompileTime

data PropertyAlias = forall b. (:>) b String

{-| A function similar to 'from', but allowing you to specify the table name and field names.
 @
   [|| Q.fromSchema @TestEntity "ent" (\l -> [l.s Q.:> "another_name"]) ||]
 @
-}
fromSchema :: forall a. String -> (a -> [PropertyAlias]) -> Query a
fromSchema = onlyCompileTime

-- | Note that the predicate only accepts lambda.
filter :: (a -> Bool) -> Query a -> Query a
filter = onlyCompileTime

-- | Note that the first argument only accepts a lambda.
orderBy :: (a -> r) -> Query a -> Query a
orderBy = onlyCompileTime

{-| Note that the first argument only accepts a lambda.
    The second returned 'Query' can be used to perform aggregation queries.
 @
   [|| (\(g, t) -> (g, Q.size t)) <$> Q.groupBy (\t -> i t) (Q.from @TestEntity) ||]
 @
-}
groupBy :: (a -> r) -> Query a -> Query (r, Query a)
groupBy = onlyCompileTime

-- | Note that the third argument only accepts a lambda with two arguments.
join :: Query a -> Query b -> (a -> b -> Bool) -> Query (a, b)
join = onlyCompileTime

leftJoin :: Query a -> Query b -> (a -> b -> Bool) -> Query (a, Maybe b)
leftJoin = onlyCompileTime

rightJoin :: Query a -> Query b -> (a -> b -> Bool) -> Query (Maybe a, b)
rightJoin = onlyCompileTime

fullJoin :: Query a -> Query b -> (a -> b -> Bool) -> Query (Maybe a, Maybe b)
fullJoin = onlyCompileTime

min :: Num a => Query a -> Maybe Int
min = onlyCompileTime

max :: Num a => Query a -> Maybe Int
max = onlyCompileTime

avg :: Num a => Query a -> Maybe Int
avg = onlyCompileTime

sum :: Num a => Query a -> Int
sum = onlyCompileTime

size :: Query a -> Integer
size = onlyCompileTime

nonEmpty :: Query a -> Bool
nonEmpty = onlyCompileTime

isEmpty :: Query a -> Bool
isEmpty = onlyCompileTime

skip :: Int -> Query a -> Query a
skip = onlyCompileTime

take :: Int -> Query a -> Query a
take = onlyCompileTime

union :: Query a -> Query a -> Query a
union = onlyCompileTime

unionAll :: Query a -> Query a -> Query a
unionAll = onlyCompileTime

instance Functor Query where
  fmap :: (a -> b) -> Query a -> Query b
  fmap = onlyCompileTime

instance Applicative Query where
  -- | Can only be used at the end of a do block.
  pure = onlyCompileTime
  -- | Cannot be used.
  (<*>) = onlyCompileTime

instance Monad Query where
  (>>=) = onlyCompileTime

{-| A function that allows you to apply a predicate to a value that may be null.
    The predicate must be a lambda.
-}
exists :: Maybe a -> (a -> Bool) -> Bool
exists = onlyCompileTime

data Action a

data Assigment = forall b. (:=) b b

{-| The first parameter must be either 'from' or 'fromSchema'.
@
   [|| Q.insert (Q.from @TestEntity) (\t -> [t.s Q.:= "d", t.i Q.:= 1]) ||]
 @
-}
insert :: Query a -> (a -> [Assigment]) -> Action a
insert = onlyCompileTime

{-| The first parameter must be either 'from', or 'fromSchema', or 'filter' applied to from.
@
   [|| Q.update (Q.filter (\a -> a.s == "g") (Q.from @TestEntity)) (\t -> [t.s Q.:= t.s, t.o Q.:= Just 1]) ||]
 @
-}
update :: Query a -> (a -> [Assigment]) -> Action a
update = onlyCompileTime

-- | The first parameter must be either 'from', or 'fromSchema', or 'filter' applied to from.
delete :: Query a -> Action a
delete = onlyCompileTime

-- | Allows the use of runtime value.
lift :: a -> a
lift = onlyCompileTime

-- | The now() function from postgresql.
now :: LocalTime
now = onlyCompileTime

-- This function is needed because we cannot determine
-- what type the fmap function is applied to
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = onlyCompileTime
