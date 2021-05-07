{-# LANGUAGE DataKinds #-}

module Data.RankSelect.Maps where

import           Data.Enum.FixedList
import qualified Data.RankSelect.Map          as RSMap
import qualified Data.RankSelectArray.Class   as RSArray
import           Data.RankSelectArray.SDArray
import           Data.RankSelectArray.Union
import qualified Data.Vector                  as V

import qualified Data.Vector                  as Vector
import qualified Data.Bifunctor               as Bifunctor
import           Data.List.Utils

-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Data.Enum.FixedList
-- >>> import Data.Enum.Letter
-- >>> import Data.Enum.Utils
-- >>> import Data.DNA.Assembly

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
fromListsAscOfTwo =  fromListsAscOfTwoWith const

-- | Transform two list of (k,v) pairs into RankSelectMaps
--
-- >>> fromListsAscOfTwoWith const fromBoundedEnum 16 (unsafeFixedList @2 [[("AT", 'a'), ("GT", 'b'), ("TT", 'd')], [("AG", 'b'), ("CG", 'd'), ("TT", 'c')]] :: FixedList 2 [(FixedList 2 (Letter "ACGT"), Char)])
-- RankSelectMaps {commonPart = 00000000000000010, getListMap = [fromListAscN fromBoundedEnum 32 3 [("AT",'a'),("GT",'b'),("TT",'d')],fromListAscN fromBoundedEnum 32 3 [("AG",'b'),("CG",'d'),("TT",'c')]]}
-- >>> fromListsAscOfTwoWith (+) fromBoundedEnum 16 (unsafeFixedList @2 [[("AT", 1), ("GT", 1), ("TT", 1)], [("AG", 1), ("CG", 1), ("TT", 1)]] :: FixedList 2 [(FixedList 2 (Letter "ACGT"), Int)])
-- RankSelectMaps {commonPart = 00000000000000010, getListMap = [fromListAscN fromBoundedEnum 32 3 [("AT",1),("GT",1),("TT",1)],fromListAscN fromBoundedEnum 32 3 [("AG",1),("CG",1),("TT",1)]]}
-- >>> fromListsAscOfTwoWith (+) fromBoundedEnum 16 (unsafeFixedList @2 [[("AT", 1), ("AT", 1), ("AT", 1),("AT", 1),("GT", 1), ("TT", 1)], [("AG", 1), ("CG", 1), ("TT", 1)]] :: FixedList 2 [(FixedList 2 (Letter "ACGT"), Int)])
-- RankSelectMaps {commonPart = 00000000000000010, getListMap = [fromListAscN fromBoundedEnum 32 3 [("AT",4),("GT",1),("TT",1)],fromListAscN fromBoundedEnum 32 3 [("AG",1),("CG",1),("TT",1)]]}
fromListsAscOfTwoWith
  :: (Eq k, Eq v)
  => (v -> v -> v)
  -> (k -> Int)
  -> Int
  -> FixedList 2 [(k, v)]
  -> RankSelectMaps k v
fromListsAscOfTwoWith with toInt n kvss = fromListsEnumOfTwoWith with n (fmap (map (Bifunctor.first toInt)) kvss)

-- | Convert two lists into rank select maps
--
-- >>> fromListsEnumOfTwoWith (+) 64 (unsafeFixedList @2 [[(0,1),(1,1),(5,1),(20,1),(16,1),(1,1),(5,1)], [(0,1),(1,1),(6,1),(24,1),(32,1),(1,1),(5,1)]]) :: RankSelectMaps (Edge 2 (Letter "ACGT")) Int
-- RankSelectMaps {commonPart = 11000100000000000000000000000000000000000000000000000000000000000, getListMap = [fromListAscN fromBoundedEnum 128 5 [("AAA",1),("AAC",2),("ACC",2),("CAA",1),("CCA",1)],fromListAscN fromBoundedEnum 128 6 [("AAA",1),("AAC",2),("ACC",1),("ACG",1),("CGA",1),("GAA",1)]]}
fromListsEnumOfTwoWith
  :: (Eq k, Eq v)
  => (v -> v -> v)
  -> Int
  -> FixedList 2 [(Int, v)]
  -> RankSelectMaps k v
fromListsEnumOfTwoWith combine n (FixedList kvss) = RankSelectMaps {commonPart=commonPartArray, getListMap=maps}
  where
    maps = zipWith (\up vs -> RSMap.RankSelectMap up (Vector.fromList (map snd vs))) uniqueParts kvss'
    (Unions commonPartArray uniqueParts) = unionOfTwo
    unionOfTwo = fromListsAsc n (map fst first) (map fst second)
    kvss'@(first:(second:_)) = fmap (nubSortOnWith combine' fst) kvss
    combine' (k, v) (_, v') =  (k, v `combine` v')


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

    

