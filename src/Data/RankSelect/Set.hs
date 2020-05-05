{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.RankSelect.Set as RSSet
module Data.RankSelect.Set where

import           Data.List             (intercalate)
import qualified GHC.Exts              as GHC

import qualified Data.BitArray.Class   as BitArray
import           Data.BitArray.SDArray (SDArray')
import qualified Data.BitArray.SDArray as SDArray

import           Data.Enum.Utils
import           Data.List.Utils

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import Data.Int (Int8)

-- | 'RankSelectSet' is a set of values of type @a@
-- that supports efficient 'rank' and 'select' operations.
--
-- Many operations on 'RankSelectSet' rely on keys being 'Enum' and 'Bounded'.
data RankSelectSet a = RankSelectSet
  { rsBitmap :: SDArray' }
  deriving (Eq)

instance (Show a, Bounded a, Enum a) => Show (RankSelectSet a) where
  show rs = intercalate " "
    [ "fromListAscN fromBoundedEnum"
    , show (capacity rs)
    , show (size rs)
    , show (toListBoundedEnum rs)
    ]

-- |
--
-- >>> [3, 64, -32] :: RankSelectSet Int8
-- fromListAscN fromBoundedEnum 256 3 [-32,3,64]
instance (Bounded a, Enum a) => GHC.IsList (RankSelectSet a) where
  type Item (RankSelectSet a) = a
  fromList = fromListBoundedEnum
  toList = toListBoundedEnum

-- | \(O(?)\).
--
-- Returns number of elements in 'RankSelectSet'
-- that are less than or equal to a given element.
--
-- >>> rank 45 ([3, 64, -32] :: RankSelectSet Int8)
-- 2
--
-- >>> rank 'o' (fromListBoundedEnum "world" :: RankSelectSet Char)
-- 3
rank :: (Bounded a, Enum a) => a -> RankSelectSet a -> Int
rank x rs = BitArray.rank (rsBitmap rs) True (fromBoundedEnum x)

-- | \(O(?)\).
--
-- @'select' i rs@ returns \(i\)th least element from @rs@.
--
-- >>> select 2 ([3, 64, -32] :: RankSelectSet Int8)
-- 3
--
-- >>> select 3 (fromListBoundedEnum "world" :: RankSelectSet Char)
-- 'o'
select :: (Bounded a, Enum a) => Int -> RankSelectSet a -> a
select i rs = toBoundedEnum (BitArray.select (rsBitmap rs) True i)

-- | Capacity of 'RankSelectSet' (maximum possible number of elements).
--
-- >>> capacity ([2, 3, 5, 7] :: RankSelectSet Int8)
-- 256
capacity :: RankSelectSet a -> Int
capacity = SDArray.bitVectorSize . rsBitmap

-- | Number of elements in the set.
--
-- >>> size ([2, 3, 5, 7] :: RankSelectSet Int8)
-- 4
size :: RankSelectSet a -> Int
size = length . SDArray.lowerBits . rsBitmap

-- * Conversion to and from lists

-- ** Convert 'RankSelectSet' to list

toList :: (Int -> a) -> RankSelectSet a -> [a]
toList fromInt = map fromInt . SDArray.toOnes . rsBitmap

toListEnum :: Enum a => RankSelectSet a -> [a]
toListEnum = toList toEnum

toListBoundedEnum :: forall a. (Bounded a, Enum a) => RankSelectSet a -> [a]
toListBoundedEnum = toList toBoundedEnum

-- ** Convert enumerations of elements to 'RankSelectSet'

fromEnumList :: Int -> [Int] -> RankSelectSet a
fromEnumList n = fromEnumListAsc n . nubSort

fromEnumListN :: Int -> Int -> [Int] -> RankSelectSet a
fromEnumListN n m = fromEnumListAscN n m . nubSort

fromEnumListAsc :: Int -> [Int] -> RankSelectSet a
fromEnumListAsc n xs = fromEnumListAscN n (length xs) xs

fromEnumListAscN :: Int -> Int -> [Int] -> RankSelectSet a
fromEnumListAscN n m = RankSelectSet . BitArray.fromOnes n m

-- ** Convert an arbitrary list to 'RankSelectSet'

-- | Construct 'RankSelectSet' of given capacity from an arbitrary list.
--
-- Duplicate values in input list are ignored.
fromList :: (a -> Int) -> Int -> [a] -> RankSelectSet a
fromList toInt n = fromListAsc toInt n . nubSortOn toInt

fromListEnum :: Enum a => Int -> [a] -> RankSelectSet a
fromListEnum = fromList fromEnum

fromListBoundedEnum :: (Bounded a, Enum a) => [a] -> RankSelectSet a
fromListBoundedEnum = fromListAscBoundedEnum . nubSortOn fromBoundedEnum

-- ** Convert an ordered list to 'RankSelectSet'

fromListAscN
  :: (a -> Int)
  -> Int
  -> Int
  -> [a]
  -> RankSelectSet a
fromListAscN toInt n m
  | n < m = error $ "this RankSelectSet cannot contain more than " <> show n <> " elements"
  | otherwise = RankSelectSet . BitArray.fromOnes n m . map toInt

fromListAsc :: (a -> Int) -> Int -> [a] -> RankSelectSet a
fromListAsc toInt n xs = fromListAscN toInt n (length xs) xs

fromListAscEnumN :: Enum a => Int -> Int -> [a] -> RankSelectSet a
fromListAscEnumN = fromListAscN fromEnum

fromListAscEnum :: Enum a => Int -> [a] -> RankSelectSet a
fromListAscEnum = fromListAsc fromEnum

fromListAscBoundedEnumN :: forall a. (Bounded a, Enum a) => Int -> [a] -> RankSelectSet a
fromListAscBoundedEnumN = fromListAscN fromBoundedEnum
  (fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1)

fromListAscBoundedEnum :: (Bounded a, Enum a) => [a] -> RankSelectSet a
fromListAscBoundedEnum xs = fromListAsc fromBoundedEnum (boundedEnumSize xs) xs
