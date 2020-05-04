{-# LANGUAGE DeriveFoldable      #-}
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

import           Data.List             (intercalate)
import           Data.Proxy
import           Data.Vector           (Vector)
import qualified Data.Vector           as Vector
import qualified GHC.Exts              as GHC

import qualified Data.BitArray.Class   as BitArray
import           Data.BitArray.SDArray (SDArray')
import qualified Data.BitArray.SDArray as SDArray

import           Data.Enum.Utils
import           Data.List.Utils

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import Data.Int (Int8)

-- | 'RankSelectMap' is k key-value dictionary
-- that supports efficient 'rank' and 'select' operations on keys.
--
-- Many operations on 'RankSelectMap' rely on keys being 'Enum' and 'Bounded'.
data RankSelectMap k v = RankSelectMap
  { rsBitmap :: SDArray'
  , rsValues :: Vector v
  } deriving (Eq, Functor, Foldable, Traversable)

instance (Show v, Show k, Bounded k, Enum k) => Show (RankSelectMap k v) where
  show rs = intercalate " "
    [ "fromListAscN fromBoundedEnum"
    , show (capacity rs)
    , show (size rs)
    , show (toListBoundedEnum rs)
    ]

-- |
--
-- >>> [(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap Int8 Char
-- fromListAscN fromBoundedEnum 256 3 [(-32,'c'),(3,'a'),(64,'b')]
instance (Bounded k, Enum k) => GHC.IsList (RankSelectMap k v) where
  type Item (RankSelectMap k v) = (k, v)
  fromList = fromListBoundedEnum
  toList = toListBoundedEnum

-- | \(O(?)\).
--
-- Returns number of keys in 'RankSelectMap'
-- that are less than or equal to k given key.
--
-- >>> rank 45 ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap Int8 Char)
-- 2
rank :: (Bounded k, Enum k) => k -> RankSelectMap k v -> Int
rank x rs = BitArray.rank (rsBitmap rs) True (fromBoundedEnum x)

-- | \(O(?)\).
--
-- @'select' i rs@ returns \(i\)th least key from @rs@
-- along with its associated value.
--
-- >>> select 2 ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap Int8 Char)
-- (3,'a')
select :: (Bounded k, Enum k) => Int -> RankSelectMap k v -> (k, v)
select i rs = (toBoundedEnum k, rsValues rs Vector.! (i - 1))
  where
    k = BitArray.select (rsBitmap rs) True i

-- | Capacity of 'RankSelectMap' (maximum possible number of elements).
--
-- >>> capacity ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap Int8 Char)
-- 256
capacity :: RankSelectMap k v -> Int
capacity = SDArray.bitVectorSize . rsBitmap

-- | Number of elements in the set.
--
-- >>> size ([(3, 'a'), (64, 'b'), (-32, 'c')] :: RankSelectMap Int8 Char)
-- 3
size :: RankSelectMap k v -> Int
size = length . rsValues

-- * Conversion to and from lists

-- ** Convert 'RankSelectMap' to list

keys :: (Int -> k) -> RankSelectMap k v -> [k]
keys fromInt = map fromInt . SDArray.toOnes . rsBitmap

elems :: RankSelectMap k v -> [v]
elems = Vector.toList . rsValues

toList :: (Int -> k) -> RankSelectMap k v -> [(k, v)]
toList fromInt m = zip (keys fromInt m) (elems m)

toListEnum :: Enum k => RankSelectMap k v -> [(k, v)]
toListEnum = toList toEnum

toListBoundedEnum :: (Bounded k, Enum k) => RankSelectMap k v -> [(k, v)]
toListBoundedEnum = toList toBoundedEnum

-- ** Convert an arbitrary list to 'RankSelectMap'

fromList :: (k -> Int) -> Int -> [(k, v)] -> RankSelectMap k v
fromList = fromListWith const

fromListEnum :: Enum k => Int -> [(k, v)] -> RankSelectMap k v
fromListEnum = fromListEnumWith const

fromListBoundedEnum :: (Bounded k, Enum k) => [(k, v)] -> RankSelectMap k v
fromListBoundedEnum = fromListBoundedEnumWith const

fromListWith :: (v -> v -> v) -> (k -> Int) -> Int -> [(k, v)] -> RankSelectMap k v
fromListWith combine toInt n = fromListAsc toInt n . nubSortOnWith combine' (toInt . fst)
  where
    combine' (k, v) (_, v') = (k, v `combine` v')

fromListEnumWith :: Enum k => (v -> v -> v) -> Int -> [(k, v)] -> RankSelectMap k v
fromListEnumWith combine = fromListWith combine fromEnum

fromListBoundedEnumWith
  :: (Bounded k, Enum k)
  => (v -> v -> v)
  -> [(k, v)]
  -> RankSelectMap k v
fromListBoundedEnumWith combine
  = fromListAscBoundedEnum . nubSortOnWith combine' (fromBoundedEnum . fst)
  where
    combine' (k, v) (_, v') = (k, v `combine` v')

-- ** Convert an ordered list to 'RankSelectMap'

fromListAscN
  :: (k -> Int)
  -> Int
  -> Int
  -> [(k, v)]
  -> RankSelectMap k v
fromListAscN toInt n m kvs
  | n < m = error $ "this RankSelectMap cannot contain more than " <> show n <> " elements"
  | otherwise = RankSelectMap
    { rsBitmap = BitArray.fromOnes n m (map (toInt . fst) kvs)
    , rsValues = Vector.fromList (map snd kvs)
    }

fromListAsc :: (k -> Int) -> Int -> [(k, v)] -> RankSelectMap k v
fromListAsc toInt n xs = fromListAscN toInt n (length xs) xs

fromListAscEnumN :: Enum k => Int -> Int -> [(k, v)] -> RankSelectMap k v
fromListAscEnumN = fromListAscN fromEnum

fromListAscEnum :: Enum k => Int -> [(k, v)] -> RankSelectMap k v
fromListAscEnum = fromListAsc fromEnum

fromListAscBoundedEnumN
  :: forall k v. (Bounded k, Enum k) => Int -> [(k, v)] -> RankSelectMap k v
fromListAscBoundedEnumN = fromListAscN fromBoundedEnum
  (fromEnum (maxBound :: k) - fromEnum (minBound :: k) + 1)

fromListAscBoundedEnum :: forall k v. (Bounded k, Enum k) => [(k, v)] -> RankSelectMap k v
fromListAscBoundedEnum xs = fromListAsc fromBoundedEnum (boundedEnumSize (Proxy @k)) xs
