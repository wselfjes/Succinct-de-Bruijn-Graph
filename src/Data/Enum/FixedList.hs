{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Enum.FixedList where

import           Data.Proxy
import           Data.String     (IsString (..))
import           GHC.TypeLits

import           Data.Enum.Utils
import           Data.List.Utils (chunksOf)

-- $setup
-- >>> :set -XDataKinds

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

instance (KnownNat n, Bounded a, Enum a) => Enum (FixedList n a) where
  fromEnum (FixedList xs) = sum (zipWith fromSymbol xs [0..])
    where
      fromSymbol x i = fromBoundedEnum x * (base ^ i)
      base = boundedEnumSize xs

  toEnum i = FixedList (unpadded <> replicate pad minBound)
    where
      n = fromIntegral (natVal (Proxy @n))
      pad = n - length unpadded
      unpadded = go i
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

-- |
-- >>> fixedChunks "Hello!" :: [FixedList 5 Char]
-- [FixedList {getFixedList = "Hello"},FixedList {getFixedList = "ello!"}]
fixedChunks :: forall n a. KnownNat n => [a] -> [FixedList n a]
fixedChunks = map FixedList . chunksOf n
  where
    n = fromIntegral (natVal (Proxy :: Proxy n))

-- | Efficiently compute enum values of 'FixedList' chunks.
--
-- @
-- fixedBoundedEnumChunks = map fromEnum . fixedChunks
-- @
fixedBoundedEnumChunks
  :: forall n a. (KnownNat n, Bounded a, Enum a) => Proxy n -> [a] -> [Int]
fixedBoundedEnumChunks _ ys = go 0 [] ys
  where
    go _ _ [] = []
    go _ [] xs = go (fromEnum (FixedList @n zs)) is xs'
      where
        is = map fromBoundedEnum zs
        (zs, xs') = splitAt n xs
    go j (i:is) (x:xs) = j : go j' is xs
      where
        j' = (j - i) `div` base + fromBoundedEnum x * base ^ (n - 1)

    n = fromIntegral (natVal (Proxy :: Proxy n))
    base = boundedEnumSize ys
