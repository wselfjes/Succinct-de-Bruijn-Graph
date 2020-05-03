module Data.BitArrays.BitArray where


type BitArraySize = Int


-- | Class for represent Bit Array, may be implicit bit array such as Vector Bool, or explicit like sdarrays
class BitArray a where
  generateEmpty :: BitArraySize -> a       -- ^ Generate bit array with all 0
  setBits       :: a -> [(Int, Bool)] -> a -- ^ Generate bit array from edges
  select        :: a -> Bool -> Int -> Int
  rank          :: a -> Bool -> Int -> Int

