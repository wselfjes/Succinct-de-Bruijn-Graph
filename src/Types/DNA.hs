{-# LANGUAGE InstanceSigs #-}

module Types.DNA where

import           Prelude hiding (seq)

-- | DNASequence is list of Alphabet
newtype DNASequence =
  DNASequence [Alphabet]
  deriving (Eq, Show)

-- | DNASequence is a semigroup
-- >>> DNASequence [T,T] <> DNASequence [T,A] <> DNASequence [A,C]
-- DNASequence [T,T,A,C]
-- >>> DNASequence [T,T] <> (DNASequence [T,A] <> DNASequence [A,C])
-- DNASequence [T,T,A,C]
-- >>> (DNASequence [T,T] <> DNASequence [T,A]) <> DNASequence [A,C]
-- DNASequence [T,T,A,C]
instance Semigroup DNASequence where
  (<>) :: DNASequence -> DNASequence -> DNASequence
  (<>) (DNASequence []) seq2 = seq2
  (<>) seq1 (DNASequence []) = seq1
  (<>) (DNASequence seq1) (DNASequence seq2) =
    DNASequence $ seq1 <> findTail seq1 seq2
    where
      findTail _ [] = []
      findTail [] seq2' = seq2'
      findTail seq1' seq2' =
        if last seq1' == head seq2'
          then findTail (init seq1') (tail seq2')
          else seq2'

-- | DNASequence is a monoid.
-- Empty element is empty list
instance Monoid DNASequence where
  mempty = DNASequence []

-- | Alphabet is Enum of possible values.
-- A is Adenine
-- C is Cytosine
-- G is Guanine
-- T is Thymine
data Alphabet
  = A
  | C
  | G
  | T
  deriving (Enum, Eq, Show)

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
