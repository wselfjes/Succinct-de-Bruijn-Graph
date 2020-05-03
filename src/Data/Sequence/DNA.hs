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
--newtype DNASequence = DNASequence { getDNASequence :: Sequence Nucleotide }
--  deriving (Eq, Generic)
type DNASequence = Sequence Nucleotide

--newtype DNASequence = DNASequence { getDNASequence :: [Nucleotide] }
--  deriving (Eq, Generic)

instance (Show a) => Show (Sequence a) where
  show = concatMap show . getSequence

-- |
-- >>> fromString "ACTG" :: DNASequence
-- ACTG
instance (IsString a) => IsString (Sequence a) where
  fromString = Sequence . map fromString . map (\x -> [x])

instance IsString Nucleotide where
  fromString = unsafeCharToNucleotide . (head)

-- |
unsafeParseDNASequence :: String -> DNASequence
unsafeParseDNASequence = Sequence . map unsafeCharToNucleotide

unsafeCharToNucleotide :: Char -> Nucleotide
unsafeCharToNucleotide 'A' = A
unsafeCharToNucleotide 'C' = C
unsafeCharToNucleotide 'G' = G
unsafeCharToNucleotide 'T' = T
unsafeCharToNucleotide c = error $
    "Invalide nucleotide " <> show c <> " (should be one of " <> show allNucleotides <> ")"

allNucleotides :: [Nucleotide]
allNucleotides = [minBound..maxBound]

mergeDNASequence :: DNASequence -> DNASequence -> DNASequence
mergeDNASequence (Sequence l) (Sequence r) = Sequence (merge l r [])
  where
    merge xs ys []
      | xs == ys = xs ++ ys
    merge xs ys prefix
      | xs == take lenXs ys = prefix ++ ys
      | otherwise = merge (drop 1 xs) ys (prefix ++ take 1 xs)
        where
          lenXs = length xs

-- | Separate sequence to overlaping chunks
chunk ::
     Int -- ^ Length of chunk
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
splitByN ::
     Int -- ^ Length of subsequence
  -> Sequence a -- ^ Original sequence
  -> [Sequence a] -- ^ List of subsequences
splitByN n (Sequence seq) = map Sequence (chunk n seq)

-- | Sequence to number
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

-- | Number to sequence
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
