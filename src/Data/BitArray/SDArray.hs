-- |
--
-- This is an implementation of a succinct bit array
-- based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.
module Data.BitArray.SDArray where

import           Data.BitArray.Class
import           Data.BitArray.VectorBitArray
import qualified Data.IntMap                  as IntMap
import qualified Data.Vector                  as Vec

-- |
data SDArray = SDArray
  { lowerBits     :: Vec.Vector Int
  , upperBits     :: VectorBitArray
  , ones          :: [Int]
  , bitVectorSize :: BitArraySize
  }

instance BitArray SDArray where
  generateEmpty = sdarrayGenerateEmpty
  setBits       = sdarraySetBits
  select        = sdarraySelect
  rank          = sdarrayRank


sdarrayGenerateEmpty
  :: BitArraySize
  -> SDArray
sdarrayGenerateEmpty _ = SDArray
                              { lowerBits = Vec.generate 0 (const 0)
                              , upperBits = generateEmpty 0
                              , ones = [] , bitVectorSize = 0}

sdarraySetBits
  :: SDArray
  -> [(Int, Bool)]
  -> SDArray
sdarraySetBits sdarray elems = fromOnes newOnes
  where
    intMapOnes = IntMap.fromList (zip (ones sdarray) (repeat True))
    intMapElems = IntMap.fromList elems
    newOnesMap = intMapElems `IntMap.union` intMapOnes
    newOnes = map (fst) (filter (snd) (IntMap.toList newOnesMap))

fromOnes
  :: [Int]
  -> SDArray
fromOnes onesPos = SDArray
                        { lowerBits     = newLowerBits
                        , upperBits     = newUpperBits
                        , ones          = onesPos
                        , bitVectorSize = ceiling n
                        }
  where
    m = fromIntegral (length onesPos) :: Float
    n = fromIntegral (onesPos !! (ceiling (m - 1))) :: Float
    offsetLowerBit = ceiling (logBase 2 (n / m))
    lowerBitsList = map (getLowerBits offsetLowerBit) onesPos
    upperBitsList = map (getUpperBits offsetLowerBit) onesPos
    upperBitsPos = zip (map (uncurry (+)) (zip [0..] upperBitsList)) (repeat True)
    newUpperBits = generateEmpty (2 * (ceiling m)) `setBits` upperBitsPos
    newLowerBits = Vec.fromList lowerBitsList

getUpperBits
  :: Int
  -> Int
  -> Int
getUpperBits skipNumber value = floor ((fromIntegral value) / (2 ^ (fromIntegral skipNumber)))

getLowerBits
  :: Int
  -> Int
  -> Int
getLowerBits offset value = value - ((getUpperBits offset value) * 2 ^ (fromIntegral offset))

sdarraySelect
  :: SDArray
  -> Bool
  -> Int
  -> Int
sdarraySelect (SDArray lwBits upBits onesPos size) _ pos = ((select upBits True pos) - (pos - 1)) * 2 ^ w + (lwBits Vec.! (pos - 1))
  where
    m = fromIntegral (length onesPos) :: Float
    n = fromIntegral size :: Float
    w = ceiling (logBase 2 (n / m))

sdarrayRank
  :: SDArray
  -> Bool
  -> Int
  -> Int
sdarrayRank (SDArray lwBits upBits onesPos size) _ pos = getPos y' x'
  where
    m = fromIntegral (length onesPos) :: Float
    n = fromIntegral size :: Float
    w = ceiling (logBase 2 (n / m))
    y' = (select (upBits) False (getUpperBits w pos)) + 1
    x' = y' - (getUpperBits w pos)
    j = getLowerBits w pos
    getPos y x
      | not ((getVec upBits) Vec.! y) = x
      | lwBits Vec.! x >= j = if lwBits Vec.! x == j then x + 1 else x
      | otherwise = getPos (y + 1) (x + 1)

