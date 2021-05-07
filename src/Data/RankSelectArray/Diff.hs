module Data.RankSelectArray.Diff where

import           Data.Maybe
import           Data.RankSelectArray.Class


-- $setup
-- >>> import Data.RankSelectArray.SDArray (SDArray')

data Diff a b = Diff a b
    deriving (Show)


instance (RankSelectArray a, RankSelectArray b) => RankSelectArray (Diff a b) where
  select diff forOnes count = fromMaybe (-1) $ diffSelect diff forOnes count
  rank (Diff left right) forOnes pos = rank left forOnes pos - rank right forOnes pos
  generateEmpty s = Diff (generateEmpty s) (generateEmpty s)
  setBits (Diff left right) bits = Diff (setBits left bits) right
  getSize (Diff left _) = getSize left
  getOneCount (Diff left right) = getOneCount left - getOneCount right


-- ** Consturctors

-- | Make Diff out of two lists of ones positions, for better storage performance |Right| << |Left|
--
-- >>> fromListsAsc 5 [1, 2, 3, 4] [1, 2] :: Diff SDArray' SDArray'
-- Diff 011110 011000
-- >>> fromListsAsc 5 [1, 2, 3, 4] [] :: Diff SDArray' SDArray'
-- Diff 011110 000000
fromListsAsc
  :: (RankSelectArray a, RankSelectArray b)
  => Int   -- ^ Size of RankSelectArray
  -> [Int] -- ^ Left ones
  -> [Int] -- ^ Right ones
  -> Diff a b
fromListsAsc size leftOnes rightOnes = diff
  where
    diff = Diff (fromOnes size (length leftOnes) leftOnes) (fromOnes size (length rightOnes) rightOnes)

fromRankSelectArrays :: a -> b -> Diff a b
fromRankSelectArrays = Diff


-- ** Query operations

-- | Select operation on diff
--
-- >>> diffSelect (fromListsAsc 5 [1, 2, 3, 4] [1, 2] :: Diff SDArray' SDArray') True 1
-- Just 3
-- >>> diffSelect (fromListsAsc 5 [1, 2, 3, 4] [1, 2] :: Diff SDArray' SDArray') True 3
-- Nothing
-- >>> diffSelect (fromListsAsc 5 [1, 2, 3, 4] [] :: Diff SDArray' SDArray') True 1
-- Just 1
-- >>> diffSelect (fromListsAsc 5 [1, 2, 3, 4] [] :: Diff SDArray' SDArray') True 3
-- Just 3
diffSelect
  :: (RankSelectArray a, RankSelectArray b)
  => Diff a b
  -> Bool
  -> Int
  -> Maybe Int
diffSelect diff@(Diff left right) forOnes count
  | count < 0 = Nothing
  | forOnes && count > getOneCount diff = Nothing
  | not forOnes && count > getSize diff - getOneCount diff = Nothing
  | otherwise = Just (recursion count 0)
  where
    recursion i' previousRank
      | currentRank == previousRank = currentSelect
      | otherwise = recursion (i' + currentRank - previousRank) currentRank
      where
        currentSelect = select left forOnes i'
        currentRank = rank right forOnes currentSelect
