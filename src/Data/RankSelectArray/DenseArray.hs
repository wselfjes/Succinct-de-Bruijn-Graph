{-# LANGUAGE RecordWildCards #-}
-- |
--
-- This is an implementation of a Dense Array based on the paper [Practical Entropy-Compressed Rank/Select Dictionary](https://arxiv.org/abs/cs/0610001) by Daisuke Okanohara and Kunihiko Sadakane.

module Data.RankSelectArray.DenseArray where

import           Data.List.Split
import           Data.RankSelectArray.Class
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
    , pIndexes        :: V.Vector PIndex
    , indexes         :: V.Vector (V.Vector Index)
    , reversePIndexes :: V.Vector PIndex
    , reverseIndexes  :: V.Vector (V.Vector Index)
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
denseGenerateEmpty size = DenseArray (generateEmpty size) size V.empty V.empty V.empty V.empty 

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
  -> V.Vector PIndex
generatePIndexes storage l forOnes = V.fromList (map PIndex (pIndexes ++ [getSize storage - 1]))
  where
    numberOfOnes = fromIntegral (getOneCount storage) :: Float
    numberOfRequiredElems = if forOnes
                           then numberOfOnes
                           else ((fromIntegral . getSize) storage) - numberOfOnes
    pIndexSize = ceiling (numberOfRequiredElems / fromIntegral l) :: Int
    pIndexes = map (\i -> select storage forOnes (i * l + 1)) [0 .. pIndexSize-1]

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
      | i > unPIndex end = []
      | otherwise = if getBit storage i == forOnes
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
      | getBit storage i == forOnes = i : go (i + 1)
      | otherwise = go (i + 1)

-- | Generate two indexes. Ss for dense block and Sl for sparse block
generateIndexes
  :: RankSelectArray storage
  => storage
  -> V.Vector PIndex
  -> Bool
  -> V.Vector (V.Vector Index)
generateIndexes storage pIndexes forOnes = V.fromList (map V.fromList indexes)
  where
    indexes = map (\i -> generateBlock (pIndexes V.! (i - 1), pIndexes V.! i)) [1 .. length pIndexes]
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
    smallPIndexes = (V.uniq . V.takeWhile (<= pos) . V.map unPIndex) pIndexesTemp
    pIndex = V.last smallPIndexes
    indexOfP = length smallPIndexes - 1
    numberFromPIndexes = l1 * indexOfP
    numberFromIndexes = if unPIndex (pIndexesTemp V.! (indexOfP + 1)) - pIndex < l2  
                        then (length . V.takeWhile (<=pos) . V.map unIndex) (indexesTemp V.! indexOfP)
                        else l3 * (length . V.takeWhile (<=pos) . V.map unIndex) (indexesTemp V.! indexOfP)

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
    pIndex = pIndexesTemp V.! indexOfP
    pos = if unPIndex (pIndexesTemp V.! (indexOfP + 1)) - unPIndex pIndex < l2 
          then searchInSlBlock (indexesTemp V.! indexOfP) (i - indexOfP * l1 - 1) 
          else searchInSsBlock (indexesTemp V.! indexOfP) storage (i - indexOfP * l1 - 1) forOnes

searchInSlBlock
  :: V.Vector Index
  -> Int
  -> Index
searchInSlBlock indexes i = indexes V.! i

searchInSsBlock
  :: RankSelectArray storage
  => V.Vector Index
  -> storage
  -> Int
  -> Bool
  -> Index
searchInSsBlock indexes storage ind forOnes = pos
  where
    i = ind - l3Index * l3 + 1
    pos = Index (go 0 l3Value)
    l3Index = floor (fromIntegral ind / fromIntegral l3)
    l3Value = unIndex (indexes V.! l3Index)
    go accumulator i'
      | accumulator == i = i' - 1
      | getBit storage i' == forOnes = go (accumulator + 1) (i' + 1)
      | otherwise = go accumulator (i' + 1)


