module Data.BitArray.Class where

type BitArraySize = Int

-- | Class for represent Bit Array, may be implicit bit array such as Vector Bool, or explicit like sdarrays
class BitArray a where
  select        :: a -> Bool -> Int -> Int
  rank          :: a -> Bool -> Int -> Int

  generateEmpty :: BitArraySize -> a       -- ^ Generate bit array with all 0
  setBits       :: a -> [(Int, Bool)] -> a -- ^ Generate bit array from edges

  -- | Construct a bit-array from positions of 1s.
  -- Default implementation relies on 'generateEmpty' and 'setBits'
  -- however a more efficient implementation migth be used.
  fromOnes      :: BitArraySize   -- ^ Size of an array.
                -> Int            -- ^ Number of 1s.
                -> [Int]          -- ^ Positions of 1s in a bit-array.
                -> a
  fromOnes n _ ones = generateEmpty n `setBits` bits
    where
      bits = map (\i -> (i, True)) ones

  -- | Get bit at a given index. Default implementation relies on 'rank' and 'select',
  -- however a more efficient implementation can often be used.
  getBit :: Int -> a -> Bool
  getBit i a = select a False k /= k
    where
      k = rank a False i
