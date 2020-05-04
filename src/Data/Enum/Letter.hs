{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Enum.Letter where

import           Data.List    (nub)
import           Data.Proxy
import qualified Data.Vector  as Vector
import           GHC.TypeLits

-- * Alphabet from a type-level string

newtype Letter (alphabet :: Symbol) = Letter { getLetter :: Char }
  deriving (Eq, Ord, Show)

unsafeLetters :: forall alphabet. KnownSymbol alphabet => String -> [Letter alphabet]
unsafeLetters = map unsafeLetter

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

