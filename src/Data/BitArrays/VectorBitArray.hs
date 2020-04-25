module Data.BitArrays.VectorBitArray where

import qualified Data.Vector as Vec
import           Data.BitArrays.BitArray 
import           Data.Function      (on)


newtype VectorBitArray = VectorBitArray { getVec :: Vec.Vector Bool }


instance BitArray VectorBitArray where
  generateEmpty size = VectorBitArray (Vec.generate size (const False))
  setBits            = setBits'
  select             = select'
  rank               = rank'

instance Eq VectorBitArray where
  (==) = (==) `on` (getVec)

instance Show VectorBitArray where
  show = show . getVec


-- | Use update Vector function
setBits' :: VectorBitArray -> [(Int, Bool)] -> VectorBitArray
setBits' (VectorBitArray vec) listValues = VectorBitArray (vec Vec.// listValues)

-- | Returns the position of the i-th occurrence of 1
select' 
  :: VectorBitArray -- ^ Bit array
  -> Bool
  -> Int            -- ^ i-th occurrence of 1
  -> Int            -- ^ Position of i-th occurrence of 1 in bit array
select' (VectorBitArray bitArr') val i = (select'' bitList i 0) - 1
  where
    select'' [] _ ind = ind
    select'' _ 0 ind
      | otherwise   = ind
    select'' (val':xs) i' ind
      | val' == val = select'' xs (i' - 1) (ind + 1)
      | otherwise   = select'' xs i' (ind + 1)
    bitList = Vec.toList bitArr'

-- | Returns the number of elements equal to 1 up to position i
rank' 
  :: VectorBitArray -- ^ Bit array
  -> Bool
  -> Int            -- ^ Position i in bit Array
  -> Int            -- ^ Number of ones up to position i
rank' (VectorBitArray bitArr') val i = sum $ take (i + 1) bitList
  where
    bitList =
      map
        (\x ->
           if x == val
             then 1
             else 0)
        (Vec.toList bitArr')
