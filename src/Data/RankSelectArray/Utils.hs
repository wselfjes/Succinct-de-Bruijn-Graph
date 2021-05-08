module Data.RankSelectArray.Utils where

import           Data.List

import qualified Data.RankSelectArray.Class as Class

-- $setup
-- >>> import Data.RankSelectArray.VectorBitArray

-- | Get intersection of two rank select arrays
--
-- >>> ((Class.fromOnes 10 3 [1,3,5] :: VectorBitArray) `intersection` (Class.fromOnes 10 3 [1,4,5] :: VectorBitArray)) :: VectorBitArray
-- 0100010000
intersection
  :: (Class.RankSelectArray a, Class.RankSelectArray b, Class.RankSelectArray c)
  => a
  -> b
  -> c
intersection first second = Class.fromOnes size (length ones) ones
  where
    size = Class.getSize first
    ones = Class.toOnes first `intersect` Class.toOnes second


-- | Difference between two arrays
--
-- >>> ((Class.fromOnes 10 3 [1,3,5] :: VectorBitArray) `difference` (Class.fromOnes 10 3 [1,4,5] :: VectorBitArray)) :: VectorBitArray
-- 0001000000
difference
  :: (Class.RankSelectArray a, Class.RankSelectArray b, Class.RankSelectArray c)
  => a
  -> b
  -> c
difference first second = Class.fromOnes size (length ones) ones
  where
    size = Class.getSize first
    ones = Class.toOnes first \\ Class.toOnes second
