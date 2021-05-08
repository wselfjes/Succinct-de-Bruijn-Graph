{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}
module Data.Sequence.DNA where

import           Data.String  (IsString (..))
import           GHC.Generics (Generic)
import           Prelude      hiding (seq)

-- $setup
-- >>> :set -XOverloadedStrings

-- | Sequence is abstract sequence of arbitrary alphabet
newtype Sequence a = Sequence { getSequence :: [a]}
  deriving (Eq, Generic, Ord)

-- | DNA nucleotide.
data Nucleotide = A
    | C
    | G
    | T
    deriving (Enum, Eq, Show, Bounded, Ord)

-- | DNA is a sequence of 'Nucleotide's.
type DNASequence = Sequence Nucleotide

instance (Show a) => Show (Sequence a) where
  show = concatMap show . getSequence

-- | Convert string to DNASequence
-- >>> fromString "ACTG" :: DNASequence
-- ACTG
instance (IsString a) => IsString (Sequence a) where
  fromString = Sequence . map fromString . map (\x -> [x])

-- | Convert string to Nucleotide
instance IsString Nucleotide where
  fromString = unsafeCharToNucleotide . (head)

unsafeParseDNASequence :: String -> DNASequence
unsafeParseDNASequence = Sequence . map unsafeCharToNucleotide

unsafeCharToNucleotide :: Char -> Nucleotide
unsafeCharToNucleotide 'A' = A
unsafeCharToNucleotide 'C' = C
unsafeCharToNucleotide 'G' = G
unsafeCharToNucleotide 'T' = T
unsafeCharToNucleotide c = error $
    "Invalide nucleotide " <> show c <> " (should be one of " <> show allNucleotides <> ")"

-- | List of all nucleotides
allNucleotides :: [Nucleotide]
allNucleotides = [minBound..maxBound]

-- | Merge DNA sequence as a overlapping strings
-- >>> mergeDNASequence "ACGTA" "GTAC"
-- ACGTAC
mergeDNASequence
  :: DNASequence -- ^ Left string
  -> DNASequence -- ^ Right string
  -> DNASequence -- ^ Merged strings
mergeDNASequence (Sequence l) (Sequence r) = Sequence (merge l r [])
  where
    merge xs ys []
      | xs == ys = xs ++ ys
    merge xs ys prefix
      | xs == take lenXs ys = prefix ++ ys
      | otherwise = merge (drop 1 xs) ys (prefix ++ take 1 xs)
        where
          lenXs = length xs

-- | Separate sequence to overlapping chunks
chunk
  :: Int -- ^ Length of chunk
  -> [a] -- ^ Original list
  -> [[a]] -- ^ List of chunks
chunk n xs =
  if length chunk' < n
    then []
    else chunk' : chunk n (tail xs)
  where
    chunk' = take n xs

-- | Separate DNASequence to overlaping chunks
-- Wrapper of chunk
splitByN
  :: Int -- ^ Length of subsequence
  -> Sequence a -- ^ Original sequence
  -> [Sequence a] -- ^ List of subsequences
splitByN n (Sequence seq) = map Sequence (chunk n seq)

-- | Convert sequence to number. 
-- Sequence can be presented as n-base number. 
-- So this method converts from n-base to 10-base number.
sequenceToNumber :: (Enum a) => Sequence a -> Int
sequenceToNumber (Sequence seq) =
  sum $
  map
    (\(num, letter) -> base ^ num * (fromEnum letter))
    (zip [seqLength,(seqLength - 1) .. 0] seq)
  where
    seqLength = length seq - 1
    maxNumber = maxBound :: Nucleotide
    base = fromEnum maxNumber + 1

-- | Convert number to sequence. 
-- Sequence can be presented as n-base number. 
-- So this method converts from 10-base to n-base number.
numberToSequence
  :: (Enum a)
  => Int -- ^ Length of result sequences
  -> Int -- ^ Original number
  -> Sequence a -- ^ Resulting sequence
numberToSequence p x = (Sequence . reverse . take p . num2Seq') x
  where
    num2Seq' 0 = [toEnum 0 | _ <- [1 .. p]]
    num2Seq' y =
      let (a, b) = quotRem y 4
       in toEnum b : num2Seq' a

-- | Get node in which edge goes
getToNode :: DNASequence -> Int
getToNode (Sequence seq') = sequenceToNumber $ Sequence (tail seq')

-- | Get node from which edge goes
getFromNode :: DNASequence -> Int
getFromNode (Sequence seq') = sequenceToNumber $ Sequence (init seq')
