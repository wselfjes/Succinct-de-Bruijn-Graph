-- |
--
-- This is an implementation of a Dense Array based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.
 
module Data.RankSelectArray.DenseArray where

import           Data.RankSelectArray.Class
import           Data.RankSelectArray.VectorBitArray
import qualified Data.Vector as V


l1 :: Int
l1 = 2^10
l2 :: Int
l2 = 2^16
l3 :: Int
l3 = 2^5

data DenseArray = DenseArray 
  { pArrays :: [VectorBitArray] 
  , size    :: BitArraySize
  } deriving (Eq)


instance RankSelectArray DenseArray where
  rank          = denseRank
  select        = denseSelect
  generateEmpty = denseGenerateEmpty
  setBits       = denseSetBits
--  fromOnes      = denseFromOnes


denseRank
  :: DenseArray
  -> Bool
  -> Int
  -> Int
denseRank _ _ _ = 0

denseSelect
  :: DenseArray
  -> Bool
  -> Int
  -> Int
denseSelect _ _ _ = 0

denseGenerateEmpty
  :: BitArraySize
  -> DenseArray
denseGenerateEmpty size = DenseArray [] size

denseSetBits
  :: DenseArray
  -> [(Int, Bool)]
  -> DenseArray
denseSetBits (DenseArray _ size) ones = DenseArray pArrays size
  where
    pArrays = generatePArrays numPArrays (size `div` l1 - 1) ones []
    numPArrays = length ones `div` l1
    generatePArrays :: Int -> Int -> [(Int, Bool)] -> [VectorBitArray] -> [VectorBitArray]
    generatePArrays 0 _ _ pArrays' = pArrays'
    generateParrays numPArrays' pArraySize ones' pArrays' = pArrays'
    
-- denseFromOnes
--   :: RankSelectArray arr
--   => BitArraySize
--   -> Int
--   -> [Int]
--   -> arr
-- denseFromOnes size numOnes ones = array
--   where
--     array = if numOnes < L1 
--             then fromOnes size numOnes ones :: VectorBitArray
--             else denseArray
--     denseArray = denseGenerateEmpty size `setBits` bits
--     bits = map (\i -> (i, True)) ones
