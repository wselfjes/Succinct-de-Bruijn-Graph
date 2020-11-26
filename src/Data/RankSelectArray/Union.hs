module Data.RankSelectArray.Union where


import Control.Applicative
import Data.RankSelectArray.Class
import Data.Maybe


data Union a b = Union a b deriving (Eq, Show)


instance (RankSelectArray a, RankSelectArray b) => RankSelectArray (Union a b) where
  select = unionSelect
  rank = unionRank
  generateEmpty size = Union (generateEmpty size) (generateEmpty size)
  setBits arr bits = unionSetBits arr (map (\(ind, v) -> (ind, v, Left True)) bits) 
  getSize (Union left right) = getSize left + getSize right
  getOneCount (Union left right) = getOneCount left + getOneCount right


-- | Left and right arrays must be disjoint
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
      leftResult = ultimateBinarySearch left (const count) (const 0) (selectGetItem) rankCompare
      rightResult = ultimateBinarySearch right (const count) (const 0) (selectGetItem) rankCompare

      selectGetItem coll i = case select coll forOnes i of
                             -1 -> Nothing
                             v -> Just v
      rankCompare v = compare count (unionRank union forOnes v)

    

-- | Left and right arrays must be disjoint
unionRank
  :: (RankSelectArray a, RankSelectArray b)
  => Union a b
  -> Bool
  -> Int
  -> Int
unionRank (Union left right) forOnes pos = rank left forOnes pos + rank right forOnes pos



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
    filterFunction isLeft (_, _, Left _) = isLeft
    filterFunction isLeft (_, _, Right _) = not isLeft
    leftArrays = map (\(ind, value, _) -> (ind, value)) ((filter (filterFunction True)) bits)
    rightArrays = map (\(ind, value, _) -> (ind, value)) ((filter (filterFunction False)) bits)



ultimateBinarySearch 
  :: (Ord s, Num s, Integral s)
  => f                           -- * Collection
  -> (f -> s)                    -- * Get maximum selector
  -> (f -> s)                    -- * Get minimum selector
  -> (f -> s -> Maybe a)         -- * Get item
  -> (a -> Ordering)             -- * compare values, example (compare a)
  -> Maybe a
ultimateBinarySearch collection getMaxSelector getMinSelector getItem comp = recursion minSelector maxSelector
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
          midPoint = fromInteger (ceiling ((fromIntegral (left + right)) / 2))
          value = collection `getItem` midPoint
          ordering = comp <$> value

