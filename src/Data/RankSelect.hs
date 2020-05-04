{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.RankSelect where

import qualified Data.BitArray.Class   as BitArray
import           Data.BitArray.SDArray (SDArray')
import qualified Data.BitArray.SDArray as SDArray
import           Data.Function         (on)
import           Data.List             (intercalate, nub, sortBy)
import           Data.Proxy
import           Data.String           (IsString (..))
import qualified Data.Vector           as Vector
import           GHC.TypeLits

-- | 'RankSelect' is a set of values of type @a@
-- that supports efficient 'rank' and 'select' operations.
data RankSelect a = RankSelect
  { rsBitmap :: SDArray' }

instance (Show a, Bounded a, Enum a) => Show (RankSelect a) where
  show rs = intercalate " "
    [ "fromListAscBoundedEnumN"
    , show (capacity rs)
    , show (size rs)
    , show (toListBoundedEnum rs)
    ]

-- | \(O(?)\).
--
-- Returns number of elements in 'RankSelect'
-- that are less than or equal to a given element.
--
-- >>> rank 45 (fromListBoundedEnum [3, 64, -32] :: RankSelect Int8)
-- 2
--
-- >>> rank 'o' (fromListBoundedEnum "world" :: RankSelect Char)
-- 3
rank :: (Bounded a, Enum a) => a -> RankSelect a -> Int
rank x rs = BitArray.rank (rsBitmap rs) True (fromBoundedEnum x)

-- | \(O(?)\).
--
-- @'select' i rs@ returns \(i\)th least element from @rs@.
--
-- >>> select 2 (fromListBoundedEnum [3, 64, -32] :: RankSelect Int8)
-- 3
-- >>> select 3 (fromListBoundedEnum "world" :: RankSelect Char)
-- 'o'
select :: (Bounded a, Enum a) => Int -> RankSelect a -> a
select i rs = toBoundedEnum (BitArray.select (rsBitmap rs) True i)

capacity :: RankSelect a -> Int
capacity = SDArray.bitVectorSize . rsBitmap

size :: RankSelect a -> Int
size = length . SDArray.lowerBits . rsBitmap

-- * Conversion to and from lists

-- ** Convert 'RankSelect' to list

toList :: (Int -> a) -> RankSelect a -> [a]
toList fromInt = map fromInt . SDArray.toOnes . rsBitmap

toListEnum :: Enum a => RankSelect a -> [a]
toListEnum = toList toEnum

toListBoundedEnum :: forall a. (Bounded a, Enum a) => RankSelect a -> [a]
toListBoundedEnum = toList toBoundedEnum

-- ** Convert an arbitrary list to 'RankSelect'

-- | Construct 'RankSelect' of given capacity from an arbitrary list.
--
-- Duplicate values in input list are ignored.
fromList :: (a -> Int) -> Int -> [a] -> RankSelect a
fromList toInt n = fromListAsc toInt n . nubSortOn toInt

fromListEnum :: Enum a => Int -> [a] -> RankSelect a
fromListEnum = fromList fromEnum

fromListBoundedEnum :: (Bounded a, Enum a) => [a] -> RankSelect a
fromListBoundedEnum = fromListAscBoundedEnum . nubSortOn fromBoundedEnum

-- ** Convert an ordered list to 'RankSelect'

fromListAscN
  :: (a -> Int)
  -> Int
  -> Int
  -> [a]
  -> RankSelect a
fromListAscN toInt n m
  | n < m = error $ "this RankSelect cannot contain more than " <> show n <> " elements"
  | otherwise = RankSelect . BitArray.fromOnes n m . map toInt

fromListAsc :: (a -> Int) -> Int -> [a] -> RankSelect a
fromListAsc toInt n xs = fromListAscN toInt n (length xs) xs

fromListAscEnumN :: Enum a => Int -> Int -> [a] -> RankSelect a
fromListAscEnumN = fromListAscN fromEnum

fromListAscEnum :: Enum a => Int -> [a] -> RankSelect a
fromListAscEnum = fromListAsc fromEnum

fromListAscBoundedEnumN :: forall a. (Bounded a, Enum a) => Int -> [a] -> RankSelect a
fromListAscBoundedEnumN = fromListAscN fromBoundedEnum
  (fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1)

fromListAscBoundedEnum :: (Bounded a, Enum a) => [a] -> RankSelect a
fromListAscBoundedEnum xs = fromListAsc fromBoundedEnum (boundedEnumSize xs) xs

-- * Alphabet from a type-level string

newtype Letter (alphabet :: Symbol) = Letter { getLetter :: Char }
  deriving (Eq, Ord, Show)

unsafeLetter :: forall alphabet. KnownSymbol alphabet => Char -> Letter alphabet
unsafeLetter c
  | c `notElem` alphabet = error $
      "symbol " <> show c <> " is not in the alphabet " <> show alphabet <> ""
  | otherwise = Letter c
  where
    alphabet = nub (symbolVal (Proxy :: Proxy alphabet))

instance KnownSymbol alphabet => Enum (Letter alphabet) where
  toEnum n = Letter (alphabet Vector.! n)
    where
      alphabet = Vector.fromList (nub (symbolVal (Proxy :: Proxy alphabet)))
  fromEnum (Letter c) =
    case lookup c (zip alphabet [0..]) of
      Just n  -> n
      Nothing -> error $
        "symbol " <> show c <> " is not in the alphabet " <> show alphabet <> ""
    where
      alphabet = nub (symbolVal (Proxy :: Proxy alphabet))

instance KnownSymbol alphabet => Bounded (Letter alphabet) where
  minBound = Letter (head alphabet)
    where
      alphabet = nub (symbolVal (Proxy :: Proxy alphabet))
  maxBound = Letter (last alphabet)
    where
      alphabet = nub (symbolVal (Proxy :: Proxy alphabet))

-- * Lists of fixed length

newtype FixedList (n :: Nat) a = FixedList { getFixedList :: [a] }
  deriving (Eq, Ord, Show)

unsafeFixedList :: forall n a. KnownNat n => [a] -> FixedList n a
unsafeFixedList xs
  | length xs /= n = error $
      "expected list size " <> show n <> " but input list is of size " <> show (length xs)
  | otherwise = FixedList xs
  where
    n = fromIntegral (natVal (Proxy :: Proxy n))

instance (Bounded a, Enum a) => Enum (FixedList n a) where
  fromEnum (FixedList xs) = sum (zipWith fromSymbol xs [0..])
    where
      fromSymbol x i = fromBoundedEnum x * (base ^ i)
      base = boundedEnumSize xs

  toEnum n = FixedList (go n)
    where
      base = boundedEnumSize (Proxy :: Proxy a)
      go k
        | k > 0 = toBoundedEnum m : go d
        | otherwise = []
        where
          (d, m) = k `divMod` base

instance (Bounded a, KnownNat n) => Bounded (FixedList n a) where
  minBound = FixedList (replicate n minBound)
    where n = fromIntegral (natVal (Proxy :: Proxy n))
  maxBound = FixedList (replicate n maxBound)
    where n = fromIntegral (natVal (Proxy :: Proxy n))

instance KnownNat n => IsString (FixedList n Char) where
  fromString = unsafeFixedList

-- * Helpers

toBoundedEnum :: forall a. (Bounded a, Enum a) => Int -> a
toBoundedEnum i = toEnum (i + fromEnum (minBound :: a))

fromBoundedEnum :: forall a. (Bounded a, Enum a) => a -> Int
fromBoundedEnum x = fromEnum x - fromEnum (minBound :: a)

boundedEnumSize :: forall a proxy. (Bounded a, Enum a) => proxy a -> Int
boundedEnumSize _ = fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1

-- | A version of 'nubSort' which operates on a portion of the value.
--
-- > nubSortOn length ["a","test","of","this"] == ["a","of","test"]
--
-- From https://hackage.haskell.org/package/extra
nubSortOn :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn f = nubSortBy (compare `on` f)

-- | A version of 'nubSort' with a custom predicate.
--
-- > nubSortBy (compare `on` length) ["a","test","of","this"] == ["a","of","test"]
--
-- From https://hackage.haskell.org/package/extra
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp = f . sortBy cmp
    where f (x1:x2:xs) | cmp x1 x2 == EQ = f (x1:xs)
          f (x:xs)     = x : f xs
          f []         = []
