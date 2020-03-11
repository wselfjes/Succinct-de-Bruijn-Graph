{-# LANGUAGE InstanceSigs #-}

module Types.DNA where

import           Prelude hiding (seq)

newtype DNASequence =
  DNASequence [Alphabet]
  deriving (Eq, Show)

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

instance Monoid DNASequence where
  mempty = DNASequence []

data Alphabet
  = A
  | C
  | G
  | T
  deriving (Enum, Eq, Show)

chunk :: Int -> [a] -> [[a]]
chunk n xs =
  if length chunk' < n
    then []
    else chunk' : chunk n (tail xs)
  where
    chunk' = take n xs

splitByN :: Int -> DNASequence -> [DNASequence]
splitByN n (DNASequence seq) = map DNASequence (chunk n seq)

sequenceToNumber :: DNASequence -> Int
sequenceToNumber (DNASequence seq) =
  sum $
  map
    (\(num, letter) -> 4 ^ num * fromEnum letter)
    (zip [seqLength,(seqLength - 1) .. 0] seq)
  where
    seqLength = length seq - 1

numberToSequence :: Int -> Int -> DNASequence
numberToSequence p x = DNASequence $ reverse $ take p $ num2Seq' x
  where
    num2Seq' 0 = [toEnum 0 | _ <- [1 .. p]]
    num2Seq' y =
      let (a, b) = quotRem y 4
       in toEnum b : num2Seq' a

getToNode :: DNASequence -> Int
getToNode (DNASequence seq') = sequenceToNumber $ DNASequence (tail seq')

getFromNode :: DNASequence -> Int
getFromNode (DNASequence seq') = sequenceToNumber $ DNASequence (init seq')
