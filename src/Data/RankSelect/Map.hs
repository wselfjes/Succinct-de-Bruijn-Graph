{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.RankSelect.Map as RSMap
module Data.RankSelect.Map where

import           Data.List                    (intercalate)
import           Data.Proxy
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import qualified GHC.Exts                     as GHC

import qualified Data.RankSelectArray.Class   as RSArray
import           Data.RankSelectArray.SDArray (SDArray')

import           Data.Enum.Utils
import           Data.List.Utils

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import Data.Int (Int8)

-- | 'RankSelectMap' is k key-value dictionary
-- that supports efficient 'rank' and 'select' operations on keys.
--
-- Many operations on 'RankSelectMap' rely on keys being 'Enum' and 'Bounded'.
data RankSelectMap t k v = RankSelectMap
  { rsBitmap :: t
  , rsValues :: Vector v
  } deriving (Eq, Functor, Foldable, Traversable)

type RankSelectMap' = RankSelectMap SDArray'

instance (Show v, Show k, Bounded k, Enum k, RSArray.RankSelectArray t) => Show (RankSelectMap t k v) where
  show rs = intercalate " "
    [ "fromListAscN fromBoundedEnum"
    , show (capacity rs)
    , show (size rs)
    , show (toListBoundedEnum rs)
    ]

-- |
--
-- >>> [(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap' Int8 Char
-- fromListAscN fromBoundedEnum 256 3 [(-32,'c'),(3,'a'),(64,'b')]
instance (Bounded k, Enum k, RSArray.RankSelectArray t) => GHC.IsList (RankSelectMap t k v) where
  type Item (RankSelectMap t k v) = (k, v)
  fromList = fromListBoundedEnum
  toList = toListBoundedEnum

-- | \(O(?)\).
--
-- Returns number of keys in 'RankSelectMap'
-- that are less than or equal to a given key.
--
-- >>> rank 45 ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap' Int8 Char)
-- 2
rank :: (Bounded k, Enum k, RSArray.RankSelectArray t) => k -> RankSelectMap t k v -> Int
rank x rs = RSArray.rank (rsBitmap rs) True (fromBoundedEnum x)

-- | \(O(?)\).
--
-- @'select' i rs@ returns \(i\)th least key from @rs@
-- along with its associated value.
--
-- >>> select 2 ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap' Int8 Char)
-- (3,'a')
select :: (Bounded k, Enum k, RSArray.RankSelectArray t) => Int -> RankSelectMap t k v -> (k, v)
select i rs = (toBoundedEnum k, rsValues rs Vector.! (i - 1))
  where
    k = RSArray.select (rsBitmap rs) True i

-- | \(O(?)\).
--
-- Returns number of keys in 'RankSelectMap'
-- that are less than or equal to a given key (converted to 'Int').
--
-- >>> rankEnum (127 + 45) ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap' Int8 Char)
-- 2
rankEnum
  :: (RSArray.RankSelectArray t)
  => Int
  -> RankSelectMap t k v
  -> Int
rankEnum k rs = RSArray.rank (rsBitmap rs) True k

-- | \(O(?)\).
--
-- @'select' i rs@ returns \(i\)th least key (as 'Int') from @rs@
-- along with its associated value.
--
-- >>> selectEnum 2 ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap' Int8 Char)
-- (131,'a')
selectEnum :: (Bounded k, Enum k, RSArray.RankSelectArray t) => Int -> RankSelectMap t k v -> (Int, v)
selectEnum i rs = (k, rsValues rs Vector.! (i - 1))
  where
    k =  RSArray.select (rsBitmap rs) True i

-- | Capacity of 'RankSelectMap' (maximum possible number of elements).
--
-- >>> capacity ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap' Int8 Char)
-- 256
capacity :: (RSArray.RankSelectArray t) => RankSelectMap t k v -> Int
capacity = RSArray.getSize . rsBitmap

-- | Number of elements in the set.
--
-- >>> size ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap' Int8 Char)
-- 3
size :: RankSelectMap t k v -> Int
size = length . rsValues

-- * Conversion to and from lists

-- ** Convert 'RankSelectMap' to list

keys :: (RSArray.RankSelectArray t) => (Int -> k) -> RankSelectMap t k v -> [k]
keys fromInt = map fromInt . RSArray.toOnes . rsBitmap

elems :: RankSelectMap t k v -> [v]
elems = Vector.toList . rsValues

toList :: (RSArray.RankSelectArray t) => (Int -> k) -> RankSelectMap t k v -> [(k, v)]
toList fromInt m = zip (keys fromInt m) (elems m)

toListEnum :: (RSArray.RankSelectArray t) => Enum k => RankSelectMap t k v -> [(k, v)]
toListEnum = toList toEnum

toListBoundedEnum :: (Bounded k, Enum k, RSArray.RankSelectArray t) => RankSelectMap t k v -> [(k, v)]
toListBoundedEnum = toList toBoundedEnum

-- ** Convert enumerations of elements to 'RankSelectSet'

fromEnumList :: (RSArray.RankSelectArray t) =>Int -> [(Int, v)] -> RankSelectMap t k v
fromEnumList = fromEnumListWith const

fromEnumListWith :: (RSArray.RankSelectArray t) =>(v -> v -> v) -> Int -> [(Int, v)] -> RankSelectMap t k v
fromEnumListWith combine n = fromEnumListAsc n . nubSortOnWith combine' fst
  where
    combine' (k, v) (_, v') = (k, v `combine` v')

fromEnumListAsc :: (RSArray.RankSelectArray t) =>Int -> [(Int, v)] -> RankSelectMap t k v
fromEnumListAsc n kvs = fromEnumListAscN n (length kvs) kvs

fromEnumListAscN :: (RSArray.RankSelectArray t) => Int -> Int -> [(Int, v)] -> RankSelectMap t k v
fromEnumListAscN n m kvs
  | n < m = error $ "this RankSelectMap cannot contain more than " <> show n <> " elements"
  | otherwise = RankSelectMap
    { rsBitmap = RSArray.fromOnes n m (map fst kvs)
    , rsValues = Vector.fromList (map snd kvs)
    }

-- ** Convert an arbitrary list to 'RankSelectMap'

fromList :: (RSArray.RankSelectArray t) =>(k -> Int) -> Int -> [(k, v)] -> RankSelectMap t k v
fromList = fromListWith const

fromListEnum :: (Enum k, RSArray.RankSelectArray t) => Int -> [(k, v)] -> RankSelectMap t k v
fromListEnum = fromListEnumWith const

fromListBoundedEnum :: (Bounded k, Enum k, RSArray.RankSelectArray t) => [(k, v)] -> RankSelectMap t k v
fromListBoundedEnum = fromListBoundedEnumWith const

fromListWith :: (RSArray.RankSelectArray t) =>(v -> v -> v) -> (k -> Int) -> Int -> [(k, v)] -> RankSelectMap t k v
fromListWith combine toInt n = fromListAsc toInt n . nubSortOnWith combine' (toInt . fst)
  where
    combine' (k, v) (_, v') = (k, v `combine` v')

fromListEnumWith :: (Enum k, RSArray.RankSelectArray t) => (v -> v -> v) -> Int -> [(k, v)] -> RankSelectMap t k v
fromListEnumWith combine = fromListWith combine fromEnum

fromListBoundedEnumWith
  :: (Bounded k, Enum k, RSArray.RankSelectArray t)
  => (v -> v -> v)
  -> [(k, v)]
  -> RankSelectMap t k v
fromListBoundedEnumWith combine
  = fromListAscBoundedEnum . nubSortOnWith combine' (fromBoundedEnum . fst)
  where
    combine' (k, v) (_, v') = (k, v `combine` v')

-- ** Convert an ordered list to 'RankSelectMap'

fromListAscN
  :: (RSArray.RankSelectArray t)
  => (k -> Int)
  -> Int
  -> Int
  -> [(k, v)]
  -> RankSelectMap t k v
fromListAscN toInt n m kvs
  | n < m = error $ "this RankSelectMap cannot contain more than " <> show n <> " elements"
  | otherwise = RankSelectMap
    { rsBitmap = RSArray.fromOnes n m (map (toInt . fst) kvs)
    , rsValues = Vector.fromList (map snd kvs)
    }

fromListAsc :: (RSArray.RankSelectArray t) =>(k -> Int) -> Int -> [(k, v)] -> RankSelectMap t k v
fromListAsc toInt n xs = fromListAscN toInt n (length xs) xs

fromListAscEnumN :: (RSArray.RankSelectArray t) =>Enum k => Int -> Int -> [(k, v)] -> RankSelectMap t k v
fromListAscEnumN = fromListAscN fromEnum

fromListAscEnum :: (RSArray.RankSelectArray t) =>Enum k => Int -> [(k, v)] -> RankSelectMap t k v
fromListAscEnum = fromListAsc fromEnum

fromListAscBoundedEnumN
  :: forall t k v. (Bounded k, Enum k, RSArray.RankSelectArray t) => Int -> [(k, v)] -> RankSelectMap t k v
fromListAscBoundedEnumN = fromListAscN fromBoundedEnum
  (fromEnum (maxBound :: k) - fromEnum (minBound :: k) + 1)

fromListAscBoundedEnum :: forall t k v. (Bounded k, Enum k, RSArray.RankSelectArray t) => [(k, v)] -> RankSelectMap t k v
fromListAscBoundedEnum xs = fromListAsc fromBoundedEnum (boundedEnumSize (Proxy @k)) xs


-- * Update

-- | Update element at key k
--
-- >>> update (subtract 1) 'a' ([('a', 3), ('b', 2), ('c', 8)] :: RankSelectMap' Char Int)
-- fromListAscN fromBoundedEnum 1114112 3 [('a',2),('b',2),('c',8)]
update
  :: (Enum k, Bounded k, RSArray.RankSelectArray t)
  => (v -> v)           -- ^ Function to get to new value
  -> k                  -- ^ Key
  -> RankSelectMap t k v  -- ^ Original map
  -> RankSelectMap t k v  -- ^ Result map
update f k rs@(RankSelectMap _ values) = rs'
  where
    rs' = rs {rsValues = values Vector.// [(i - 1, f oldValue)]}
    oldValue = snd (select i rs)
    i = rank k rs

