module Data.BitArray.Class where

type BitArraySize = Int

-- | Class for represent Bit Array, may be implicit bit array such as Vector Bool, or explicit like sdarrays
class BitArray a where
  select        :: a -> Bool -> Int -> Int
  rank          :: a -> Bool -> Int -> Int

  generateEmpty :: BitArraySize -> a       -- ^ Generate bit array with all 0
  setBits       :: a -> [(Int, Bool)] -> a -- ^ Generate bit array from edges

  -- | Get bit at a given index. Default implementation relies on 'rank' and 'select',
  -- however a more efficient implementation can often be used.
  getBit :: Int -> a -> Bool
  getBit i a = select a False k /= k
    where
      k = rank a False i
