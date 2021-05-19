module Data.RankSelectArray.Utils where


import qualified Data.RankSelectArray.Class as Class

-- $setup
-- >>> import Data.RankSelectArray.VectorBitArray
-- >>> import Data.RankSelectArray.SDArray (SDArray')

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
    ones = filter (Class.getBit first) (Class.toOnes second)


-- | Difference between two arrays
--
-- >>> ((Class.fromOnes 11 7 [0,1,2,4,6,8,10] :: SDArray') `difference` (Class.fromOnes 11 2 [1,2] :: SDArray')) :: SDArray'
-- 100010101010
difference
  :: (Class.RankSelectArray a, Class.RankSelectArray b, Class.RankSelectArray c)
  => a
  -> b
  -> c
difference first second = Class.fromOnes size (length ones) ones
  where
    size = Class.getSize first
    ones = filter (not . Class.getBit second) (Class.toOnes first)
