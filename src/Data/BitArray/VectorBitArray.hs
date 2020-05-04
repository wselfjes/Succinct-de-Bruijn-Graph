{-# LANGUAGE FlexibleContexts #-}
-- |
--
-- TODO: module description
module Data.BitArray.VectorBitArray where

import           Data.BitArray.Class
import           Data.Maybe          (fromMaybe)
import           Data.String         (IsString (..))
import qualified Data.Vector         as V

newtype VectorBitArray = VectorBitArray
  { getVec :: V.Vector Bool }
  deriving (Eq)

-- | Convert bit array into a list of 'Bool' values.
--
-- >>> toList "01101010"
-- [False,True,True,False,True,False,True,False]
toList :: VectorBitArray -> [Bool]
toList = V.toList . getVec

instance BitArray VectorBitArray where
  generateEmpty size = VectorBitArray (V.generate size (const False))
  setBits            = flip setBits'
  select arr q i     = fromMaybe (-1) (select' q i arr)
  rank arr q i       = rank' q i arr
  getBit i arr       = getVec arr V.! i

instance Show VectorBitArray where
  show = foldMap (\bit -> if bit then "1" else "0") . getVec

-- | For convenience we provide 'IsString' instance for 'VectorBitArray':
--
-- >>> "01101010" :: VectorBitArray
-- 01101010
instance IsString VectorBitArray where
  fromString = unsafeParseVectorBitArray

-- | Unsafe parser, to enable convenient 'IsString' instance.
unsafeParseVectorBitArray :: String -> VectorBitArray
unsafeParseVectorBitArray = VectorBitArray . V.fromList . map unsafeBitFromChar

-- | Unsafe parser, to enable convenient 'IsString' instance for 'VectorBitArray'.
unsafeBitFromChar :: Char -> Bool
unsafeBitFromChar '0' = False
unsafeBitFromChar '1' = True
unsafeBitFromChar c   = error ("invalid character for bit value: " <> show c)

-- | Update specific bits in a 'VectorBitArray'.
--
-- >>> setBits' [(2, False), (4, True)] "01100" :: VectorBitArray
-- 01001
setBits' :: [(Int, Bool)] -> VectorBitArray -> VectorBitArray
setBits' listValues (VectorBitArray vec) = VectorBitArray (vec V.// listValues)

-- | @'select'' q i@ returns the position of the \(i\)th occurrence of @q@ in @a@.
--
-- >>> select' False 2 "00101" -- 2nd occurence of 0
-- Just 1
--
-- >>> select' True 2 "00101" -- 2nd occurence of 1
-- Just 4
select'
  :: Bool           -- ^ Are we looking for occurrences of 1?
                    -- If 'False' — we are looking for occurrencing of 0.
  -> Int            -- ^ \(i > 0\) — occurence of value to look for.
  -> VectorBitArray
  -> Maybe Int      -- ^ Position of \(i\)-th occurrence of 1 (if exists).
select' _ 0 = const Nothing
select' val i
  = elemAt (i - 1)            -- safely index of given occurenct No.
  . map fst                   -- keep only indices
  . filter ((== val) . snd)   -- filter by value
  . zip [0..]                 -- enumerate values
  . toList                    -- convert to list

  where
    elemAt :: Int -> [a] -> Maybe a
    elemAt j = safeHead . drop j

    safeHead :: [a] -> Maybe a
    safeHead []    = Nothing
    safeHead (x:_) = Just x

-- | @'rank'' q i@ the number of elements equal to @q@ up to position \(i\).
--
-- >>> rank' False 5 "01101010" -- count 0s up to 5th position in array
-- 3
--
-- >>> rank' True 3 "01101010" -- count 1s up to 3rd position in array
-- 2
rank'
  :: Bool           -- ^ Are we counting 1s? If 'False' — we are counting 0s.
  -> Int            -- ^ Position \(i\) in bit Array. \(0 \leq i < N\).
  -> VectorBitArray -- ^ Input bit array.
  -> Int            -- ^ Number of 1s up to position \(i\).
rank' val i = length . filter (== val) . take (i + 1) . toList

