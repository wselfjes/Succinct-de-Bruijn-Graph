{-# LANGUAGE RecordWildCards #-}
-- |
--
-- This is an implementation of a Dense Array based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.

module Data.RankSelectArray.DenseArray where

import           Data.List.Split
import           Data.List.Unique
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


newtype PIndex = PIndex {unPIndex :: Int}
    deriving (Eq, Show)

newtype Index = Index {unIndex :: Int}
    deriving (Eq, Show)

data DenseArray storage = DenseArray
    { storage         :: storage
    , size            :: BitArraySize
    , pIndexes        :: [PIndex]
    , indexes         :: [[Index]]
    , reversePIndexes :: [PIndex]
    , reverseIndexes  :: [[Index]]
    }
    deriving (Eq, Show)

instance RankSelectArray storage => RankSelectArray (DenseArray storage) where
  rank          = denseRank
  select        = denseSelect
  generateEmpty = denseGenerateEmpty
  setBits       = denseSetBits
  getSize       = size
  getOneCount   = getOneCount . storage

-- * Constructors 

-- | Generate empty dense array.
denseGenerateEmpty
  :: RankSelectArray storage
  => BitArraySize
  -> DenseArray storage
denseGenerateEmpty size = DenseArray (generateEmpty size) size [] [] [] []

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
denseSetBits DenseArray{..} bits = DenseArray storage size pIndexes indexes reversePIndexes reverseIndexes
  where
    ones = map fst (filter snd bits)
    storage = fromOnes size (length ones) ones
    pIndexes = generatePIndexes storage l1 True
    indexes = generateIndexes storage pIndexes True
    reversePIndexes = generatePIndexes storage l1 False
    reverseIndexes = generateIndexes storage reversePIndexes False

-- | Partition bit array int
generatePIndexes
  :: RankSelectArray storage
  => storage
  -> Int -- ^ Number of ones in the block
  -> Bool
  -> [PIndex]
generatePIndexes storage l forOnes = map PIndex ((go 0) ++ [getSize storage - 1])
  where
    numberOfOnes = fromIntegral (getOneCount storage) :: Float
    numberOfRequiredElems = if forOnes
                           then numberOfOnes
                           else ((fromIntegral . getSize) storage) - numberOfOnes
    pIndexSize = ceiling (numberOfRequiredElems / fromIntegral l) :: Int
    go i
      | i >= pIndexSize = []
      | otherwise = select storage forOnes (i * l + 1) : go (i + 1)

-- | Sl is a simple position of ones
blockToSl
  :: RankSelectArray storage
  => storage
  -> (PIndex, PIndex) -- ^ Start and end of block
  -> Bool
  -> [Index]
blockToSl storage (start, end) forOnes = map Index (go (unPIndex start))
  where
    go i
      | i > (unPIndex end) = []
      | otherwise = if getBit i storage == forOnes
                    then i : go (i + 1)
                    else go (i + 1)

-- | Ss position of each l3 one.
blockToSs
  :: RankSelectArray storage
  => storage
  -> (PIndex, PIndex) -- ^ Start and end of block
  -> Bool
  -> [Index]
blockToSs storage (start, end) forOnes = map Index (concatMap (take (l3 - 1)) (chunksOf l3 (go (unPIndex start))))
  where
    go i
      | i >= (unPIndex end) = []
      | getBit i storage == forOnes = i : go (i + 1)
      | otherwise = go (i + 1)

-- | Generate two indexes. Ss for dense block and Sl for sparse block
generateIndexes
  :: RankSelectArray storage
  => storage
  -> [PIndex]
  -> Bool
  -> [[Index]]
generateIndexes storage pIndexes forOnes = go 1
  where
    go :: Int -> [[Index]]
    go i
      | i >= length pIndexes = []
      | otherwise = generateBlock (pIndexes !! (i - 1), pIndexes !! i): go (i + 1)
        where
            generateBlock (start, end) = if (unPIndex end) - (unPIndex start) < l2 
                                        then blockToSl storage (start, end) forOnes 
                                        else blockToSs storage (start, end) forOnes

-- | Rank in dense array. 
denseRank
  :: RankSelectArray storage
  => DenseArray storage
  -> Bool
  -> Int
  -> Int
denseRank DenseArray{..} forOnes pos = rank storage forOnes pos --number
  where
    number = numberFromPIndexes + numberFromIndexes
    pIndexesTemp = if forOnes
                   then pIndexes
                   else reversePIndexes
    indexesTemp = if forOnes
                  then indexes
                  else reverseIndexes
    smallPIndexes = (uniq . takeWhile (<= pos) . map unPIndex) pIndexesTemp
    pIndex = last smallPIndexes
    indexOfP = length smallPIndexes - 1
    numberFromPIndexes = l1 * indexOfP
    numberFromIndexes = if unPIndex (pIndexesTemp !! (indexOfP + 1)) - pIndex < l2  
                        then (length . takeWhile (<=pos) . map unIndex) (indexesTemp !! indexOfP)
                        else l3 * (length . takeWhile (<=pos) . map unIndex) (indexesTemp !! indexOfP)

-- | Select in dense array.
denseSelect
  :: RankSelectArray storage
  => DenseArray storage
  -> Bool
  -> Int
  -> Int
denseSelect _ _ 0 = -1
denseSelect DenseArray{..} forOnes i = unIndex pos
  where
    pIndexesTemp = if forOnes
                   then pIndexes
                   else reversePIndexes
    indexesTemp = if forOnes
                  then indexes
                  else reverseIndexes
    indexOfP = floor (fromIntegral i / fromIntegral l1)
    pIndex = pIndexesTemp !! indexOfP
    pos = if unPIndex (pIndexesTemp !! (indexOfP + 1)) - unPIndex pIndex < l2 
          then searchInSlBlock (indexesTemp !! indexOfP) (i - indexOfP * l1 - 1) 
          else searchInSsBlock (indexesTemp !! indexOfP) storage (i - indexOfP * l1 - 1) forOnes

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
  -> Bool
  -> Index
searchInSsBlock indexes storage ind forOnes = pos
  where
    i = ind - l3Index * l3 + 1
    pos = Index (go 0 l3Value)
    l3Index = floor (fromIntegral ind / fromIntegral l3)
    l3Value = unIndex (indexes !! l3Index)
    go accumulator i'
      | accumulator == i = i' - 1
      | getBit i' storage == forOnes = go (accumulator + 1) (i' + 1)
      | otherwise = go accumulator (i' + 1)


