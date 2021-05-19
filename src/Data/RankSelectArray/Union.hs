module Data.RankSelectArray.Union where


import           Control.Applicative
import           Data.Maybe
import qualified Data.RankSelect.Set          as S
import           Data.RankSelectArray.Class
import           Data.RankSelectArray.Diff    as Diff
import           Data.RankSelectArray.Utils

-- $setup
-- >>> import Data.RankSelectArray.SDArray (SDArray')

-- | Disjoint Union of two RankSelectArray
data Union a b = Union a b
    deriving (Eq, Show)

--- |
data Unions a b c = Unions a [Union b c]
  deriving (Eq, Show)

-- | Combination of Union and Diff structure
type UnionDiff a b c = Union a (Diff b c)

-- | Combination of Unions and Diff structure
type UnionsDiff a b c = Unions a b (Diff a c)


instance (RankSelectArray a, RankSelectArray b) => RankSelectArray (Union a b) where
  select = unionSelect
  rank = unionRank
  generateEmpty size = Union (generateEmpty size) (generateEmpty size)
  setBits arr bits = unionSetBits arr (map (\(ind, v) -> (ind, v, Left True)) bits)
  getSize (Union left right) = getSize left + getSize right
  getOneCount (Union left right) = getOneCount left + getOneCount right

-- ** Convert two ordered list to 'Unions'

-- | Convert two asc list to Unions Structure
-- The intersection of two arrays must be greater than 50%
--
-- >>> Data.RankSelectArray.Union.fromLists 10 [[4, 6, 8, 10],[1, 2, 3, 5, 7, 9]] :: UnionsDiff SDArray' SDArray' SDArray'
-- Unions 00000000000 [Union 00001010101 (Diff 00000000000 00000000000),Union 01110101010 (Diff 00000000000 00000000000)]
--
-- >>> Data.RankSelectArray.Union.fromLists 10 [[0, 1, 2, 4, 6, 8, 10],[1, 2, 3, 5, 7, 9]] :: UnionsDiff SDArray' SDArray' SDArray'
-- Unions 01100000000 [Union 10001010101 (Diff 01100000000 00000000000),Union 00010101010 (Diff 01100000000 00000000000)]
fromLists
  :: (RankSelectArray a, RankSelectArray c)
  => Int  -- ^ Size
  -> [[Int]] -- ^ Ones
  -> UnionsDiff a a c
fromLists size [] = Unions (generateEmpty size) []
fromLists size [ones] = Unions (fromOnes size (length ones) ones) [Union (fromOnes size (length ones) ones) (Diff.fromListsAsc size ones [])]
fromLists size (xs:(ys:rest)) = unions
  where
    unions = foldr addArrayToUnions union rest
    union = Unions commonArray [Union leftArray commonDiffArray , Union rightArray commonDiffArray]
    rightArray = S.rsBitmap rightPart
    leftArray = S.rsBitmap leftPart
    rightPart = ysSet `S.difference` commonPart
    leftPart = xsSet `S.difference` commonPart
    commonDiffArray = Diff.fromListsAsc size (toOnes commonArray) []
    commonArray = S.rsBitmap commonPart
    commonPart = xsSet `S.intersection` ysSet
    ysSet = S.fromEnumList size ys
    xsSet = S.fromEnumList size xs


-- | Convert two rankSelect arrays into UnionsDiff
--
-- >>> Data.RankSelectArray.Union.fromRankSelectArrays (fromOnes 10 3 [1, 2, 3] :: SDArray') (fromOnes 10 3 [3, 4, 5] :: SDArray') :: UnionsDiff SDArray' SDArray' SDArray'
-- Unions 00010000000 [Union 01100000000 (Diff 00010000000 00000000000),Union 00001100000 (Diff 00010000000 00000000000)]
fromRankSelectArrays
  :: (RankSelectArray arr1, RankSelectArray arr2, RankSelectArray a, RankSelectArray b, RankSelectArray c)
  => arr1
  -> arr2
  -> UnionsDiff a b c
fromRankSelectArrays first second = Unions commonArray [Union leftArray commonDiffArray , Union rightArray commonDiffArray]
  where
    commonArray = intersection first second
    leftArray = difference first commonArray
    rightArray = difference second commonArray
    commonDiffArray = Diff.fromRankSelectArrays commonArray ((generateEmpty . getSize) commonArray)


-- ** Update unions

-- | Add array into UnionsDiff, puts in front of uniqParts
--
-- >>> addArrayToUnions [0, 8] (Data.RankSelectArray.Union.fromLists 10 [[0, 1, 2, 4, 6, 8],[1, 2, 3, 5, 7, 9]]) :: UnionsDiff SDArray' SDArray' SDArray'
-- Unions 01100000000 [Union 10000000100 (Diff 01100000000 01100000000),Union 10001010100 (Diff 01100000000 00000000000),Union 00010101010 (Diff 01100000000 00000000000)]
addArrayToUnions
  :: (RankSelectArray a, RankSelectArray b, RankSelectArray c)
  => [Int] -- ^ Ones
  -> UnionsDiff a b c
  -> UnionsDiff a b c
addArrayToUnions ones (Unions commonPart uniqParts) = Unions commonPart (newUniqPart:uniqParts)
  where
  -- TODO: rewrite method with RSSets
    size = getSize commonPart
    commonOnes = toOnes commonPart
    uniquePart = filter (`notElem` commonOnes) ones
    diffPart = filter (`notElem` ones) commonOnes
    diffPartArray = fromOnes size (length diffPart) diffPart
    uniqueArray = fromOnes size (length uniquePart) uniquePart
    newUniqPart = Union uniqueArray (Diff commonPart diffPartArray)


-- ** Select Union in Unions

-- | Get by index union
--
-- >>> getUnion (Data.RankSelectArray.Union.fromLists 10 [[0, 1, 2, 4, 6, 8],[1, 2, 3, 5, 7, 9]] :: UnionsDiff SDArray' SDArray' SDArray') 0
-- Union 10001010100 (Diff 01100000000 00000000000)
getUnion
  :: Unions a b c
  -> Int
  -> Union b c
getUnion (Unions _ arr) = (!!) arr


-- ** Query operations from Union

-- | Left and right arrays must be disjoint
--
-- >>> unionSelect (getUnion (Data.RankSelectArray.Union.fromLists 10 [[0, 1, 2, 4, 6, 8],[1, 2, 3, 5, 7, 9]] :: UnionsDiff SDArray' SDArray' SDArray') 0) True 3
-- 2
unionSelect
  :: (RankSelectArray a, RankSelectArray b)
  => Union a b
  -> Bool
  -> Int
  -> Int
unionSelect union@(Union left right) forOnes count
  | count > getOneCount union || count <= 0 = -1
  | otherwise = fromMaybe (-1) (leftResult <|> rightResult)
    where
      leftResult = ultimateBinarySearch left (const 1) getOneCount selectGetItem rankCompare
      rightResult = ultimateBinarySearch right (const 1) getOneCount selectGetItem rankCompare

      selectGetItem coll i
        | selectedValue < 0 = Nothing
        | otherwise = Just selectedValue
        where
          selectedValue = select coll forOnes i
      rankCompare v = compare count (unionRank union forOnes v)



-- | Left and right arrays must be disjoint
unionRank
  :: (RankSelectArray a, RankSelectArray b)
  => Union a b
  -> Bool
  -> Int
  -> Int
unionRank (Union left right) forOnes pos = rank left forOnes pos + rank right forOnes pos


-- ** Constructor for Union

-- | Left and right arrays must be disjoint
unionSetBits
  :: (RankSelectArray a, RankSelectArray b)
  => Union a b
  -> [(Int, Bool, Either l r)]
  -> Union a b
unionSetBits (Union left right) bits = Union newLeft newRight
  where
    newLeft = setBits left leftArrays
    newRight = setBits right rightArrays
    filterFunction isLeft (_, _, Left _)  = isLeft
    filterFunction isLeft (_, _, Right _) = not isLeft
    leftArrays = map (\(ind, value, _) -> (ind, value)) (filter (filterFunction True) bits)
    rightArrays = map (\(ind, value, _) -> (ind, value)) (filter (filterFunction False) bits)

-- ** Utils

-- | Binary search for abstract collection with get and compare operations
ultimateBinarySearch
  :: (Ord s, Num s, Integral s)
  => f                           -- ^ Collection
  -> (f -> s)                    -- ^ Get minimum selector
  -> (f -> s)                    -- ^ Get maximum selector
  -> (f -> s -> Maybe a)         -- ^ Get item
  -> (a -> Ordering)             -- ^ compare values, example (compare a)
  -> Maybe a
ultimateBinarySearch collection getMinSelector getMaxSelector getItem comp = recursion minSelector maxSelector
  where
    maxSelector = getMaxSelector collection
    minSelector = getMinSelector collection
    recursion left right
      | left > right = Nothing
      | left < minSelector || right > maxSelector = Nothing
      | otherwise = case ordering of
                    Just EQ -> value
                    Just LT -> recursion left (midPoint - 1)
                    Just GT -> recursion (midPoint + 1) right
                    Nothing -> Nothing
        where
          midPoint = fromInteger (ceiling (fromIntegral (left + right) / 2))
          value = collection `getItem` midPoint
          ordering = comp <$> value

