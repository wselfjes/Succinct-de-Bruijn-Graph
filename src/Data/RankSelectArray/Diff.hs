module Data.RankSelectArray.Diff where

import Data.RankSelectArray.Class
import Data.Maybe


data Diff a b = Diff a b deriving (Show)


instance (RankSelectArray a, RankSelectArray b) => RankSelectArray (Diff a b) where
  select diff forOnes count = fromMaybe (-1) $ diffSelect diff forOnes count
  rank (Diff left right) forOnes pos = rank left forOnes pos - rank right forOnes pos 
  generateEmpty s = Diff (generateEmpty s) (generateEmpty s)
  setBits (Diff left right) bits = Diff (setBits left bits) right
  getSize (Diff left _) = getSize left
  getOneCount (Diff left right) = getOneCount left - getOneCount right


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
