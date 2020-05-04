{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Enum.FixedList where

import           Data.Proxy
import           Data.String     (IsString (..))
import           GHC.TypeLits

import           Data.Enum.Utils

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

