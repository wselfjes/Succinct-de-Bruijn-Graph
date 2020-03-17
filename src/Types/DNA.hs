{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}
module Types.DNA where

import           Data.String  (IsString (..))
import           GHC.Generics (Generic)
import           Prelude      hiding (seq)

-- $setup
-- >>> :set -XOverloadedStrings

-- | DNA is a sequence of 'Nucleotide's.
newtype DNASequence = DNASequence { getDNASequence :: [Nucleotide] }
  deriving (Eq, Generic)

instance Show DNASequence where
  show = concatMap show . getDNASequence

-- |
-- >>> getDNASequence "ACTG"
-- [A,C,T,G]
instance IsString DNASequence where
  fromString = unsafeParseDNASequence

-- |
unsafeParseDNASequence :: String -> DNASequence
unsafeParseDNASequence = DNASequence . map unsafeCharToNucleotide

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
mergeDNASequence (DNASequence l) (DNASequence r) = DNASequence (merge l r [])
  where
    merge xs ys []
      | xs == ys = xs ++ ys
    merge xs ys prefix
      | xs == take lenXs ys = prefix ++ ys
      | otherwise = merge (drop 1 xs) ys (prefix ++ take 1 xs)
        where
          lenXs = length xs

-- | DNA nucleotide.
data Nucleotide = A
    | C
    | G
    | T
    deriving (Enum, Eq, Show, Bounded)

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
  -> DNASequence -- ^ Original sequence
  -> [DNASequence] -- ^ List of subsequences
splitByN n (DNASequence seq) = map DNASequence (chunk n seq)

-- | Sequence to number
sequenceToNumber :: DNASequence -> Int
sequenceToNumber (DNASequence seq) =
  sum $
  map
    (\(num, letter) -> 4 ^ num * fromEnum letter)
    (zip [seqLength,(seqLength - 1) .. 0] seq)
  where
    seqLength = length seq - 1

-- | Number to sequence
numberToSequence ::
     Int -- ^ Length of result sequences
  -> Int -- ^ Original number
  -> DNASequence -- ^ Resulting sequence
numberToSequence p x = DNASequence $ reverse $ take p $ num2Seq' x
  where
    num2Seq' 0 = [toEnum 0 | _ <- [1 .. p]]
    num2Seq' y =
      let (a, b) = quotRem y 4
       in toEnum b : num2Seq' a

-- | Get node in which edge goes
getToNode :: DNASequence -> Int
getToNode (DNASequence seq') = sequenceToNumber $ DNASequence (tail seq')

-- | Get node from which edge goes
getFromNode :: DNASequence -> Int
getFromNode (DNASequence seq') = sequenceToNumber $ DNASequence (init seq')
