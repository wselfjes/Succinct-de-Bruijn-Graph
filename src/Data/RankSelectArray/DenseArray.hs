-- |
--
-- This is an implementation of a Dense Array based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.

module Data.RankSelectArray.DenseArray where

import           Data.RankSelectArray.Class
import           Data.RankSelectArray.VectorBitArray
import qualified Data.Vector                         as V


-- | Constants for dense array
l1 :: Int
l1 = 2^10
l2 :: Int
l2 = 2^16
l3 :: Int
l3 = 2^5

newtype PIndex = PIndex Int
    deriving (Eq, Show)

data DenseArray storage = DenseArray
    { storage  :: storage
    , size     :: BitArraySize
    , pIndexes :: [PIndex]
    }
    deriving (Eq, Show)


instance RankSelectArray storage => RankSelectArray (DenseArray storage) where
  rank          = denseRank
  select        = denseSelect
  generateEmpty = denseGenerateEmpty
  setBits       = denseSetBits
  getSize       = size
  getOneCount   = getOneCount . storage


-- | Rank in dense array. Not implemented yet.
denseRank
  :: RankSelectArray storage
  => DenseArray storage
  -> Bool
  -> Int
  -> Int
denseRank _ _ _ = 0

-- | Select in dense array. Not implemented yet.
denseSelect
  :: DenseArray storage
  -> Bool
  -> Int
  -> Int
denseSelect _ _ _ = 0

-- | Generate empty dense array.
denseGenerateEmpty
  :: RankSelectArray storage
  => BitArraySize
  -> DenseArray storage
denseGenerateEmpty size = DenseArray (generateEmpty size) size []

-- | Set bits in dense array. Create new array.
-- We first partition H into the blocks such that each block contains L ones respectively.
-- Let Pl[0 . . . n/L − 1] be the bit arrays such that Pl[i] is the position of (iL + 1)-th one.
-- We classify these blocks into two groups.
-- If the length of block size (Pl[i] − Pl[i − 1]) is larger than L2, we store all the positions of ones explicitly in Sl.
-- If the length of block size is smaller than L2, we store the each L3-th positions of ones in Ss. We can store these values in lg L2 bits.
denseSetBits
  :: RankSelectArray storage
  => DenseArray storage
  -> [(Int, Bool)]
  -> DenseArray storage
denseSetBits (DenseArray _ size pIndexes) bits = DenseArray storage size pIndexes
  where
    ones = map fst (filter snd bits)
    storage = fromOnes size (length ones) ones
    pIndexes = generatePIndexes storage l1


-- | Partition bit array int
generatePIndexes
  :: RankSelectArray storage
  => storage
  -> Int -- ^ Number of ones in the block
  -> [PIndex]
generatePIndexes storage l = map PIndex (go 0)
  where
    numberOfOnes = fromIntegral (getOneCount storage) :: Float
    pIndexSize = ceiling (numberOfOnes / fromIntegral l) :: Int
    go i
      | i >= pIndexSize = []
      | otherwise = select storage True (i * l + 1) : go (i + 1) 
