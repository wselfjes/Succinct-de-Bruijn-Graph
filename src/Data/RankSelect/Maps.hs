{-# LANGUAGE DataKinds #-}

module Data.RankSelect.Maps where

import           Data.Enum.FixedList
import qualified Data.RankSelect.Map          as RSMap
import qualified Data.RankSelectArray.Class   as RSArray
import           Data.RankSelectArray.SDArray
import           Data.RankSelectArray.Union

import qualified Data.Vector as Vector

-- | List of rank select maps over UnionDiff
data RankSelectMaps k v = RankSelectMaps
  { commonPart :: SDArray'
  , getListMap :: [RSMap.RankSelectMap (UnionDiff SDArray' SDArray' SDArray') k v]
  } deriving (Show)


fromListsAscN
  :: (Eq k, Eq v)
  => (k -> Int)
  -> Int
  -> [[(k, v)]]
  -> RankSelectMaps k v
fromListsAscN toInt n kvss
  | length kvss < 2 = error $ "this RankSelectMaps cannot contain less than 2 maps"
  | otherwise = rankSelectMaps
    where
      rankSelectMaps = fromListsAscOfTwo toInt n (unsafeFixedList $ take 2 kvss)


fromListsAscOfTwo
  :: (Eq k, Eq v)
  => (k -> Int)
  -> Int
  -> FixedList 2 [(k, v)]
  -> RankSelectMaps k v
fromListsAscOfTwo toInt n (FixedList kvss) = RankSelectMaps {commonPart=commonPartArray, getListMap=maps}
  where
    maps = zipWith (\up vs -> RSMap.RankSelectMap up (Vector.fromList (map snd vs))) uniqueParts kvss
    (Unions commonPartArray uniqueParts) = unionOfTwo
    unionOfTwo = fromListsAsc n n (map (toInt . fst) first) (map (toInt . fst) second)
    (first:(second:_)) = kvss

