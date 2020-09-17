-- |
--
-- This is an implementation of a Dense Array based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.

module Data.RankSelectArray.DenseArray where

import           Data.List.Split
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

new :: DenseArray VectorBitArray
new = setBits e [(0, True), (1, True), (2, True), (3, True), (5, True), (9, True), (12, True), (14, True)]
  where
    e = denseGenerateEmpty 16 :: DenseArray VectorBitArray


newtype PIndex = PIndex {unPIndex :: Int}
    deriving (Eq, Show)

newtype Index = Index {unIndex :: Int}
    deriving (Eq, Show)

data DenseArray storage = DenseArray
    { storage  :: storage
    , size     :: BitArraySize
    , pIndexes :: [PIndex]
    , indexes  :: [[Index]]
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
denseRank _ _ _ = error "Rank for dense array not implemented."

-- | Select in dense array. Not implemented yet.
denseSelect
  :: RankSelectArray storage
  => DenseArray storage
  -> Bool
  -> Int
  -> Int
denseSelect arr False _ = error "Select for zero values not implemented."
denseSelect (DenseArray storage size pIndexes indexes) True i' = unIndex pos
  where
    i = i' - 1
    indexOfP = floor (fromIntegral i / fromIntegral l1)
    pIndex = pIndexes !! indexOfP
    pos = if unPIndex (pIndexes !! (indexOfP + 1)) - unPIndex pIndex < l2 then searchInSlBlock (indexes !! indexOfP) (i - unPIndex pIndex) else searchInSsBlock (indexes !! indexOfP) storage (i - unPIndex pIndex + 1)

searchInSlBlock
  :: [Index]
  -> Int
  -> Index
searchInSlBlock indexes i = indexes !! i

searchInSsBlock
  :: RankSelectArray storage
  => [Index]
  -> storage
  -> Int
  -> Index
searchInSsBlock indexes storage ind = pos
  where
    i = ind - l3Index * l3 + 1
    pos = Index (go 0 l3Value)
    l3Index = floor (fromIntegral ind / fromIntegral l3)
    l3Value = unIndex (indexes !! l3Index)
    go accumulator i'
      | accumulator == i = i' - 1
      | getBit i' storage = go (accumulator + 1) (i' + 1)
      | otherwise = go accumulator (i' + 1)


-- | Generate empty dense array.
denseGenerateEmpty
  :: RankSelectArray storage
  => BitArraySize
  -> DenseArray storage
denseGenerateEmpty size = DenseArray (generateEmpty size) size [] []

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
denseSetBits (DenseArray _ size _ _) bits = DenseArray storage size pIndexes indexes
  where
    ones = map fst (filter snd bits)
    storage = fromOnes size (length ones) ones
    pIndexes = generatePIndexes storage l1
    indexes = generateIndexes storage pIndexes

-- | Partition bit array int
generatePIndexes
  :: RankSelectArray storage
  => storage
  -> Int -- ^ Number of ones in the block
  -> [PIndex]
generatePIndexes storage l = map PIndex ((go 0) ++ [getSize storage - 1])
  where
    numberOfOnes = fromIntegral (getOneCount storage) :: Float
    pIndexSize = ceiling (numberOfOnes / fromIntegral l) :: Int
    go i
      | i >= pIndexSize = []
      | otherwise = select storage True (i * l + 1) : go (i + 1)

-- | Sl is a simple position of ones
blockToSl
  :: RankSelectArray storage
  => storage
  -> (PIndex, PIndex) -- ^ Start and end of block
  -> [Index]
blockToSl storage (start, end) = map Index (go (unPIndex start))
  where
    go i
      | i >= (unPIndex end) = []
      | otherwise = if getBit i storage
                    then i : go (i + 1)
                    else go (i + 1)

-- | Ss position of each l3 one.
blockToSs
  :: RankSelectArray storage
  => storage
  -> (PIndex, PIndex) -- ^ Start and end of block
  -> [Index]
blockToSs storage (start, end) = map Index (concatMap (take (l3 - 1)) (chunksOf l3 (go (unPIndex start))))
  where
    go i
      | i >= (unPIndex end) = []
      | getBit i storage = i : go (i + 1)
      | otherwise = go (i + 1)

-- | Generate two indexes. Ss for dense block and Sl for sparse block
generateIndexes
  :: RankSelectArray storage
  => storage
  -> [PIndex]
  -> [[Index]]
generateIndexes storage pIndex = go 1
  where
    go :: Int -> [[Index]]
    go i
      | i >= length pIndex = []
      | otherwise = generateBlock (pIndex !! (i - 1), pIndex !! i): go (i + 1)
        where
            generateBlock (start, end) = if (unPIndex end) - (unPIndex start) < l2 then blockToSl storage (start, end) else blockToSs storage (start, end)

