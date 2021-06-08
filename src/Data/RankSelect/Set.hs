{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.RankSelect.Set as RSSet
module Data.RankSelect.Set where

import qualified GHC.Exts              as GHC

import qualified Data.RankSelectArray.Class   as RankSelectArray
import           Data.RankSelectArray.SDArray (SDArray')
import qualified Data.RankSelectArray.SDArray as SDArray
import qualified Data.RankSelectArray.Utils as Utils

import           Data.Enum.Utils
import           Data.List.Utils

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import Data.Int (Int8)

-- | 'RankSelectSet' is a set of values of type @a@
-- that supports efficient 'rank' and 'select' operations.
--
-- Many operations on 'RankSelectSet' rely on keys being 'Enum' and 'Bounded'.
newtype RankSelectSet t a = RankSelectSet
  { rsBitmap :: t }
  deriving (Eq)

instance (Show a, Bounded a, Enum a, RankSelectArray.RankSelectArray t) => Show (RankSelectSet t a) where
  show rs = unwords
    [ "fromListAscN fromBoundedEnum"
    , show (capacity rs)
    , show (size rs)
    , show (toListBoundedEnum rs)
    ]

-- |
--
-- >>> [3, 64, -32] :: RankSelectSet SDArray' Int8
-- fromListAscN fromBoundedEnum 256 3 [-32,3,64]
instance (Bounded a, Enum a, RankSelectArray.RankSelectArray t) => GHC.IsList (RankSelectSet t a) where
  type Item (RankSelectSet t a) = a
  fromList = fromListBoundedEnum
  toList = toListBoundedEnum

-- | \(O(?)\).
--
-- Returns number of elements in 'RankSelectSet'
-- that are less than or equal to a given element.
--
-- >>> rank 45 ([3, 64, -32] :: RankSelectSet SDArray' Int8)
-- 2
--
-- >>> rank 'o' (fromListBoundedEnum "world" :: RankSelectSet SDArray' Char)
-- 3
rank :: (Bounded a, Enum a, RankSelectArray.RankSelectArray t) => a -> RankSelectSet t a -> Int
rank x rs = RankSelectArray.rank (rsBitmap rs) True (fromBoundedEnum x)

-- | \(O(?)\).
--
-- @'select' i rs@ returns \(i\)th least element from @rs@.
--
-- >>> select 2 ([3, 64, -32] :: RankSelectSet SDArray' Int8)
-- 3
--
-- >>> select 3 (fromListBoundedEnum "world" :: RankSelectSet SDArray' Char)
-- 'o'
select :: (Bounded a, Enum a, RankSelectArray.RankSelectArray t) => Int -> RankSelectSet t a -> a
select i rs = toBoundedEnum (RankSelectArray.select (rsBitmap rs) True i)

-- | Capacity of 'RankSelectSet' (maximum possible number of elements).
--
-- >>> capacity ([2, 3, 5, 7] :: RankSelectSet SDArray' Int8)
-- 256
capacity :: (RankSelectArray.RankSelectArray t) => RankSelectSet t a -> Int
capacity = RankSelectArray.getSize . rsBitmap

-- | Number of elements in the set.
--
-- >>> size ([2, 3, 5, 7] :: RankSelectSet SDArray' Int8)
-- 4
size :: (RankSelectArray.RankSelectArray t) => RankSelectSet t a -> Int
size = RankSelectArray.getOneCount . rsBitmap

-- * Conversion to and from lists

-- ** Convert 'RankSelectSet' to list

toList :: (RankSelectArray.RankSelectArray t) => (Int -> a) -> RankSelectSet t a -> [a]
toList fromInt = map fromInt . RankSelectArray.toOnes . rsBitmap

toListEnum :: (Enum a, RankSelectArray.RankSelectArray t) => RankSelectSet t a -> [a]
toListEnum = toList toEnum

toListBoundedEnum :: forall a t. (Bounded a, Enum a, RankSelectArray.RankSelectArray t) => RankSelectSet t a -> [a]
toListBoundedEnum = toList toBoundedEnum

-- ** Convert enumerations of elements to 'RankSelectSet'

-- |
--
-- >>> rsBitmap (fromEnumList 10 [0,1,2,4,6,8,10] :: RankSelectSet SDArray' Int8)
-- 11101010101
-- >>> rsBitmap (fromEnumList 10 [1,2,3,5,7,9] :: RankSelectSet SDArray' Int8)
-- 01110101010
fromEnumList :: (RankSelectArray.RankSelectArray t) => Int -> [Int] -> RankSelectSet t a
fromEnumList n = fromEnumListAsc n . nubSort

fromEnumListN :: (RankSelectArray.RankSelectArray t) => Int -> Int -> [Int] -> RankSelectSet t a
fromEnumListN n m = fromEnumListAscN n m . nubSort

fromEnumListAsc :: (RankSelectArray.RankSelectArray t) => Int -> [Int] -> RankSelectSet t a
fromEnumListAsc n xs = fromEnumListAscN n (length xs) xs

fromEnumListAscN :: (RankSelectArray.RankSelectArray t) => Int -> Int -> [Int] -> RankSelectSet t a
fromEnumListAscN n m = RankSelectSet . RankSelectArray.fromOnes n m

-- ** Convert an arbitrary list to 'RankSelectSet'

-- | Construct 'RankSelectSet' of given capacity from an arbitrary list.
--
-- Duplicate values in input list are ignored.
fromList :: (RankSelectArray.RankSelectArray t) => (a -> Int) -> Int -> [a] -> RankSelectSet t a
fromList toInt n = fromListAsc toInt n . nubSortOn toInt

fromListEnum :: (RankSelectArray.RankSelectArray t, Enum a) => Int -> [a] -> RankSelectSet t a
fromListEnum = fromList fromEnum

fromListBoundedEnum :: (RankSelectArray.RankSelectArray t, Enum a, Bounded a) => [a] -> RankSelectSet t a
fromListBoundedEnum = fromListAscBoundedEnum . nubSortOn fromBoundedEnum

-- ** Convert an ordered list to 'RankSelectSet'

fromListAscN
  :: (RankSelectArray.RankSelectArray t) 
  => (a -> Int)
  -> Int
  -> Int
  -> [a]
  -> RankSelectSet t a
fromListAscN toInt n m
  | n < m = error $ "this RankSelectSet cannot contain more than " <> show n <> " elements"
  | otherwise = RankSelectSet . RankSelectArray.fromOnes n m . map toInt

fromListAsc :: (RankSelectArray.RankSelectArray t) => (a -> Int) -> Int -> [a] -> RankSelectSet t a
fromListAsc toInt n xs = fromListAscN toInt n (length xs) xs

fromListAscEnumN :: (RankSelectArray.RankSelectArray t, Enum a) => Int -> Int -> [a] -> RankSelectSet t a
fromListAscEnumN = fromListAscN fromEnum

fromListAscEnum :: (RankSelectArray.RankSelectArray t, Enum a) => Int -> [a] -> RankSelectSet t a
fromListAscEnum = fromListAsc fromEnum

fromListAscBoundedEnumN :: forall a t. (RankSelectArray.RankSelectArray t, Bounded a, Enum a) => Int -> [a] -> RankSelectSet t a
fromListAscBoundedEnumN = fromListAscN fromBoundedEnum
  (fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1)

fromListAscBoundedEnum :: (RankSelectArray.RankSelectArray t, Bounded a, Enum a) => [a] -> RankSelectSet t a
fromListAscBoundedEnum xs = fromListAsc fromBoundedEnum (boundedEnumSize xs) xs


-- * Operations over sets

-- | https://en.wikipedia.org/wiki/Intersection_(set_theory)
--
-- >>> rsBitmap $ intersection (fromEnumList 10 [0,1,2,4,6,8,10] :: RankSelectSet SDArray' Int8) (fromEnumList 10 [1,2,3,5,7,9] :: RankSelectSet SDArray' Int8)
-- 01100000000
intersection
  :: (RankSelectArray.RankSelectArray t)
  => RankSelectSet t a
  -> RankSelectSet t a
  -> RankSelectSet t a
intersection x y = RankSelectSet (Utils.intersection (rsBitmap x) (rsBitmap y))

-- | https://en.wikipedia.org/wiki/Complement_(set_theory)
--
-- >>> rsBitmap $ difference (fromEnumList 10 [0,1,2,4,6,8,10] :: RankSelectSet SDArray' Int8) (fromEnumList 10 [1, 2] :: RankSelectSet SDArray' Int8)
-- 10001010101
difference
  :: (RankSelectArray.RankSelectArray t)
  => RankSelectSet t a
  -> RankSelectSet t a
  -> RankSelectSet t a
difference x y = RankSelectSet (Utils.difference (rsBitmap x) (rsBitmap y))
