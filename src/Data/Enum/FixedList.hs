{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Enum.FixedList where

import           Data.Proxy
import           Data.String      (IsString (..))
import           GHC.TypeLits

import           Data.Enum.Letter
import           Data.Enum.Utils
import           Data.List.Utils  (chunksOf)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications

-- * Lists of fixed length

newtype FixedList (n :: Nat) a = FixedList { getFixedList :: [a] }
  deriving (Eq, Ord, Show, Functor)

instance {-# OVERLAPPING #-} Show (FixedList n (Letter s)) where
  show (FixedList xs) = show (map getLetter xs)

instance (KnownNat n, KnownSymbol s) => IsString (FixedList n (Letter s)) where
  fromString = unsafeFixedList . unsafeLetters

unsafeFixedList :: forall n a. KnownNat n => [a] -> FixedList n a
unsafeFixedList xs
  | length xs /= n = error $
      "expected list size " <> show n <> " but input list is of size " <> show (length xs)
  | otherwise = FixedList xs
  where
    n = fromIntegral (natVal (Proxy :: Proxy n))

instance (KnownNat n, Bounded a, Enum a) => Enum (FixedList n a) where
  fromEnum (FixedList xs) = sum (zipWith fromSymbol (reverse xs) [0..])
    where
      fromSymbol x i = fromBoundedEnum x * (base ^ i)
      base = boundedEnumSize xs

  toEnum i = FixedList (replicate pad minBound <> unpadded)
    where
      n = fromIntegral (natVal (Proxy @n))
      pad = n - length unpadded
      unpadded = reverse (go i)
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

-- | Efficiently compute enum values of 'FixedList' chunks. #TODO: FIX
--
-- @
-- fixedBoundedEnumChunks = map fromEnum . fixedChunks
-- @
--
-- prop> fixedBoundedEnumChunks @3 Proxy xs == map fromEnum (fixedChunks @3 (xs :: [Char]))
--
-- >>> fixedBoundedEnumChunks @3 Proxy ([] :: [Letter "ACGT"])
-- []
fixedBoundedEnumChunks
  :: forall n a. (KnownNat n, Bounded a, Enum a) => Proxy n -> [a] -> [Int]
fixedBoundedEnumChunks _ ys = go 0 [] ys
  where
    go _ _ []  = []
    go _ [] xs = if length firstChunk < n then [] else current : go current elValues xs'
      where
        current = fromEnum (FixedList @n firstChunk)
        (firstChunk, xs') = splitAt n xs
        elValues = fmap fromBoundedEnum firstChunk
    go current (elValue:elValues) (x:xs) = newCurrent : go newCurrent elValues' xs
      where
        newCurrent = (current - elValue * base ^ (n - 1)) * base + xN
        elValues' = elValues <> [xN]
        xN = fromBoundedEnum x
    n = fromIntegral (natVal (Proxy :: Proxy n))
    base = boundedEnumSize ys
