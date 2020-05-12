{-# LANGUAGE RecordWildCards #-}
-- |
--
-- This is an implementation of a succinct bit array
-- based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.
-- [hillbig/sdarray](https://github.com/hillbig/sdarray) used as an example
module Data.RankSelectArray.SDArray where

import           Data.RankSelectArray.Class
import           Data.RankSelectArray.VectorBitArray
--import           Data.RankSelectArray.DenseArray
import qualified Data.IntMap                  as IntMap
import qualified Data.Vector                  as Vec

import           Data.Bits                    (Bits (shiftL, shiftR, (.&.), (.|.)),
                                               FiniteBits (finiteBitSize))
import qualified Data.Bits                    as Bits

-- |
data SDArray darray = SDArray
  { bitsOffset    :: Int
  , bitVectorSize :: BitArraySize
  , upperBits     :: darray
  , lowerBits     :: Vec.Vector Int -- TODO: replace Int with [log n / m] bits
  } deriving (Eq)

countOnes :: SDArray darray -> Int
countOnes = length . lowerBits

type SDArray' = SDArray VectorBitArray

instance RankSelectArray darray => RankSelectArray (SDArray darray) where
  generateEmpty = sdarrayGenerateEmpty
  setBits       = sdarraySetBits
  select        = sdarraySelect
  rank          = sdarrayRank
  fromOnes      = sdarrayFromOnes

  -- TODO: efficient getBit implementation?

sdarrayGenerateEmpty
  :: RankSelectArray darray
  => BitArraySize
  -> SDArray darray
sdarrayGenerateEmpty _ = SDArray
  { lowerBits = Vec.generate 0 (const 0)
  , upperBits = generateEmpty 0
  , bitVectorSize = 0
  , bitsOffset = 1
  }

-- |
--
-- NOTE: this method reconstructs 'SDArray' from scratch.
sdarraySetBits
  :: RankSelectArray darray
  => SDArray darray
  -> [(Int, Bool)]
  -> SDArray darray
sdarraySetBits sdarray elems = fromOnes
  (bitVectorSize sdarray) (countOnes sdarray) newOnes
  where
    intMapOnes = IntMap.fromList (zip (toOnes sdarray) (repeat True))
    intMapElems = IntMap.fromList elems
    newOnesMap = intMapElems `IntMap.union` intMapOnes
    newOnes = map fst (filter snd (IntMap.toList newOnesMap))

toOnes :: RankSelectArray darray => SDArray darray -> [Int]
toOnes arr = map (select arr True) [1..m]
  where
    m = countOnes arr

sdarrayFromOnes
  :: RankSelectArray darray
  => BitArraySize
  -> Int
  -> [Int]            -- ^ Indices of 1s in a bit-array.
  -> SDArray darray
sdarrayFromOnes n m onesPos = SDArray
  { lowerBits     = newLowerBits
  , upperBits     = newUpperBits
  , bitVectorSize = n
  , bitsOffset    = offsetLowerBit
  }
  where
    offsetLowerBit = ceiling (logBase 2 (fromIntegral n / fromIntegral m))

    lowerBitsList = map (getLowerBits offsetLowerBit) onesPos
    newLowerBits = Vec.fromList lowerBitsList

    upperBitsList = map (getUpperBits offsetLowerBit) onesPos
    upperBitsPos = zipWith (+) [0..] upperBitsList
    newUpperBits = fromOnes (2 * m) m upperBitsPos

-- |
-- >>> getUpperBits 5 127
-- 3
getUpperBits :: Bits a => Int -> a -> a
getUpperBits skipNumber value = value `shiftR` skipNumber

-- |
-- >>> getLowerBits 5 127 :: Int
-- 31
getLowerBits :: FiniteBits a => Int -> a -> a
getLowerBits offset value = value .&. (ones `shiftR` (n - offset - 1))
  where
    n = finiteBitSize ones
    ones = Bits.complement Bits.zeroBits `Bits.clearBit` (n - 1)

sdarraySelect
  :: RankSelectArray darray
  => SDArray darray
  -> Bool
  -> Int
  -> Int
sdarraySelect _ False _ = error "select for SDArray is not implemented (for 0)"
sdarraySelect SDArray{..} True pos =
  ((select upperBits True pos - (pos - 1)) `shiftL` bitsOffset) .|. lowerBits Vec.! (pos - 1)

sdarrayRank
  :: RankSelectArray darray
  => SDArray darray
  -> Bool
  -> Int
  -> Int
sdarrayRank _ False _ = error "rank for SDArray is not implemented (for 0)"
sdarrayRank SDArray{..} True pos = getPos y' x'
  where
    upBit = getUpperBits bitsOffset pos
    y' = 1 + select upperBits False upBit
    x' = y' - upBit
    j = getLowerBits bitsOffset pos

    getPos y x
      | not (getBit y upperBits) = x
      | lowerBits Vec.! x >= j = if lowerBits Vec.! x == j then x + 1 else x
      | otherwise = getPos (y + 1) (x + 1)

