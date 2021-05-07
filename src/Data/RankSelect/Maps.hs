{-# LANGUAGE DataKinds #-}

module Data.RankSelect.Maps where

import           Data.Enum.FixedList
import qualified Data.RankSelect.Map          as RSMap
import qualified Data.RankSelectArray.Class   as RSArray
import           Data.RankSelectArray.SDArray
import           Data.RankSelectArray.Union
import qualified Data.Vector                  as V

import qualified Data.Vector                  as Vector

-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Data.Enum.FixedList
-- >>> import Data.Enum.Letter
-- >>> import Data.Enum.Utils

-- | List of rank select maps over UnionDiff
data RankSelectMaps k v = RankSelectMaps
  { commonPart :: SDArray'
  , getListMap :: [RSMap.RankSelectMap (UnionDiff SDArray' SDArray' SDArray') k v]
  } deriving (Show)


empty
  :: RankSelectMaps k v
empty = RankSelectMaps (RSArray.generateEmpty 1) []


-- | Transform list of lists into RankSelectMaps
--
-- >>> fromListsAscN fromBoundedEnum 16 ([[("AT", 'a'), ("GT", 'b'), ("TT", 'd')], [("AG", 'b'), ("CG", 'd'), ("TT", 'c')]] :: [[(FixedList 2 (Letter "ACGT"), Char)]])
-- RankSelectMaps {commonPart = 00000000000000010, getListMap = [fromListAscN fromBoundedEnum 32 3 [("AT",'a'),("GT",'b'),("TT",'d')],fromListAscN fromBoundedEnum 32 3 [("AG",'b'),("CG",'d'),("TT",'c')]]}
fromListsAscN
  :: (Eq k, Eq v)
  => (k -> Int)
  -> Int
  -> [[(k, v)]]
  -> RankSelectMaps k v
fromListsAscN toInt n kvss
  | length kvss < 2 = error "this RankSelectMaps cannot contain less than 2 maps"
  | otherwise = rankSelectMaps'
    where
      rankSelectMaps = fromListsAscOfTwo toInt n (unsafeFixedList $ take 2 kvss)
      rankSelectMaps' = foldr (addMap toInt n) rankSelectMaps (drop 2 kvss)


-- | Transform two list of (k,v) pairs into RankSelectMaps
--
-- >>> fromListsAscOfTwo fromBoundedEnum 16 (unsafeFixedList @2 [[("AT", 'a'), ("GT", 'b'), ("TT", 'd')], [("AG", 'b'), ("CG", 'd'), ("TT", 'c')]] :: FixedList 2 [(FixedList 2 (Letter "ACGT"), Char)])
-- RankSelectMaps {commonPart = 00000000000000010, getListMap = [fromListAscN fromBoundedEnum 32 3 [("AT",'a'),("GT",'b'),("TT",'d')],fromListAscN fromBoundedEnum 32 3 [("AG",'b'),("CG",'d'),("TT",'c')]]}
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
    unionOfTwo = fromListsAsc n (map (toInt . fst) first) (map (toInt . fst) second)
    (first:(second:_)) = kvss


getById
  :: RankSelectMaps k v
  -> Int
  -> RSMap.RankSelectMap (UnionDiff SDArray' SDArray' SDArray') k v
getById maps =  (!!) (getListMap maps)


-- | add map to existing set of maps
addMap
  :: (k -> Int)
  -> Int
  -> [(k, v)]
  -> RankSelectMaps k v
  -> RankSelectMaps k v
addMap toInt n kvs maps@(RankSelectMaps cp listMaps) = RankSelectMaps cp maps'
  where
    unionsDiff = toUnionsDiff maps
    newUnionsDiff = addArrayToUnions n (map (toInt . fst) kvs) unionsDiff
    newMapBitmap = getUnion newUnionsDiff 0
    newMap = RSMap.RankSelectMap newMapBitmap (V.fromList (map snd kvs))
    maps' = newMap : listMaps

-- * Convert 'RankSelectMaps' to 

toUnionsDiff
  :: RankSelectMaps k v
  -> UnionsDiff SDArray' SDArray' SDArray'
toUnionsDiff maps = Unions (commonPart maps) ((map RSMap.rsBitmap . getListMap) maps)


-- | Convert to lists
--
-- >>> toListsBoundedEnum (fromListsAscN fromBoundedEnum 16 ([[("AT", 'a'), ("GT", 'b'), ("TT", 'd')], [("AG", 'b'), ("CG", 'd'), ("TT", 'c')]] :: [[(FixedList 2 (Letter "ACGT"), Char)]]))
--[[("AT",'a'),("GT",'b'),("TT",'d')],[("AG",'b'),("CG",'d'),("TT",'c')]]
toListsBoundedEnum
  :: (Bounded k, Enum k)
  => RankSelectMaps k v
  -> [[(k, v)]]
toListsBoundedEnum = map RSMap.toListBoundedEnum . getListMap


-- | Convert two maps into RankSelectMaps
unionOfTwoMaps
  :: (Enum k, Bounded k, RSArray.RankSelectArray t)
  => RSMap.RankSelectMap t k v
  -> RSMap.RankSelectMap t k v
  -> RankSelectMaps k v
unionOfTwoMaps (RSMap.RankSelectMap arr1 values1) (RSMap.RankSelectMap arr2 values2) = maps
  where
    maps = RankSelectMaps cp listMaps
    listMaps = zipWith RSMap.RankSelectMap uniqueParts [values1, values2]
    Unions cp uniqueParts = unionOfTwo
    unionOfTwo = fromRankSelectArrays arr1 arr2

    

