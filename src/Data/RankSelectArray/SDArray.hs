{-# LANGUAGE RecordWildCards #-}
-- |
--
-- This is an implementation of a succinct bit array
-- based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.
-- [hillbig/sdarray](https://github.com/hillbig/sdarray) used as an example
module Data.RankSelectArray.SDArray where

import qualified Data.IntMap                         as IntMap
import           Data.RankSelectArray.Class
import           Data.RankSelectArray.DenseArray
import           Data.RankSelectArray.VectorBitArray

import           Data.Bits                           (Bits (shiftL, shiftR, (.&.), (.|.)),
                                                      FiniteBits (finiteBitSize))
import qualified Data.Bits                           as Bits
import           Data.Int
import           HaskellWorks.Data.AtIndex
import           HaskellWorks.Data.PackedVector
import           Prelude                             hiding (length)

-- |
data SDArray darray = SDArray
    { bitsOffset    :: Int
    , bitVectorSize :: BitArraySize
    , upperBits     :: darray
    , lowerBits     :: PackedVector64
    }
    deriving (Eq)

countOnes :: SDArray darray -> Int
countOnes = fromIntegral . toInteger . length . lowerBits

type SDArray' = SDArray VectorBitArray

instance RankSelectArray darray => Show (SDArray darray) where
  show arr = bitString
    where
      bitString = concat (map (\x -> if x then "1" else "0") bitArray)
      size = getSize arr
      bitArray = map (\x -> x `elem` (toOnes arr)) [0 .. size]

instance RankSelectArray darray => RankSelectArray (SDArray darray) where
  generateEmpty = sdarrayGenerateEmpty
  setBits       = sdarraySetBits
  select        = sdarraySelect
  rank          = sdarrayRank
  fromOnes      = sdarrayFromOnes
  getBit        = sdarrayGetBit
  getSize       = bitVectorSize
  getOneCount   = countOnes

  -- TODO: efficient getBit implementation?

-- | Create empty sdarray.
sdarrayGenerateEmpty
  :: RankSelectArray darray
  => BitArraySize
  -> SDArray darray
sdarrayGenerateEmpty _ = SDArray
  { lowerBits = empty
  , upperBits = generateEmpty 0
  , bitVectorSize = 0
  , bitsOffset = 1
  }

-- | Set bits in sdarray.
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


-- | Convert list of positions of one to sdarray.
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

    newLowerBitsList = map (fromIntegral . getLowerBits offsetLowerBit) onesPos
    lowerBitsSize = fromIntegral offsetLowerBit
    newLowerBits = fromList lowerBitsSize newLowerBitsList

    upperBitsList = map (getUpperBits offsetLowerBit) onesPos
    upperBitsPos = zipWith (+) [0..] upperBitsList
    newUpperBits = fromOnes (2 * m) m upperBitsPos

-- | Just right shift.
-- >>> getUpperBits 5 127
-- 3
getUpperBits :: Bits a => Int -> a -> a
getUpperBits skipNumber value = value `shiftR` skipNumber

-- | Replace first n bits with zero.
-- >>> getLowerBits 5 127 :: Int
-- 31
getLowerBits :: FiniteBits a => Int -> a -> a
getLowerBits offset value = value .&. (ones `shiftR` (n - offset - 1))
  where
    n = finiteBitSize ones
    ones = Bits.complement Bits.zeroBits `Bits.clearBit` (n - 1)

-- | Select in sdarray.
-- Using formula select(i, B) = (select(i, H) − i) · 2^w + L[i].
-- Where B is byte array, H array of upper bits, w number of lower bits, L array of lower bits.
sdarraySelect
  :: RankSelectArray darray
  => SDArray darray
  -> Bool
  -> Int
  -> Int
sdarraySelect _ False _ = error "select for SDArray is not implemented (for 0)"
sdarraySelect SDArray{..} True pos =
  ((select upperBits True pos - (pos - 1)) `shiftL` bitsOffset) .|. lowerBit
  where
    lowerBit = fromIntegral (toInteger (lowerBits !!! (fromIntegral (pos - 1))))

-- | Rank in sdarray.
--
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

    getPos :: Int -> Int -> Int
    getPos y x
      | not (getBit y upperBits) = x
      | (fromIntegral . toInteger) (lowerBits !!! fromIntegral x) >= j = if (fromIntegral . toInteger) (lowerBits !!! fromIntegral x) == j then x + 1 else x
      | otherwise = getPos (y + 1) (x + 1)


-- | Get bit from bit array is equal @rank ba pos@ - @rank ba (pos - 1)@
sdarrayGetBit
  :: RankSelectArray darray
  => Int
  -> SDArray darray
  -> Bool
sdarrayGetBit pos arr = (rank arr True pos) - (rank arr True (pos - 1)) /= 0
