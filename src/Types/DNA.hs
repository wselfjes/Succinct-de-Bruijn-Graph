module Types.DNA where

import           Prelude hiding (seq)

newtype DNASequence =
  DNASequence String
  deriving (Eq)

instance Show DNASequence where
  show (DNASequence string) = string

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
    (\(num, letter) -> 4 ^ num * letterToNum letter)
    (zip [seqLength,(seqLength - 1) .. 0] seq)
  where
    seqLength = length seq - 1
    letterToNum 'A' = 0
    letterToNum 'C' = 1
    letterToNum 'G' = 2
    letterToNum 'T' = 3
    letterToNum _   = -1

numberToSequence :: Int -> Int -> DNASequence
numberToSequence p x = DNASequence $ reverse $ take p $ num2Seq' x
  where
    num2Seq' 0 = [numToLetter 0 | _ <- [1 .. p]]
    num2Seq' y =
      let (a, b) = quotRem y 4
       in numToLetter b : num2Seq' a
    numToLetter 0 = 'A'
    numToLetter 1 = 'C'
    numToLetter 2 = 'G'
    numToLetter 3 = 'T'
    numToLetter _ = 'E'

(+++) :: DNASequence -> DNASequence -> DNASequence
(+++) (DNASequence "") seq2 = seq2
(+++) seq1 (DNASequence "") = seq1
(+++) (DNASequence seq1) (DNASequence seq2) = DNASequence $ seq1 ++ [last seq2]

getToNode :: DNASequence -> Int
getToNode (DNASequence seq') = sequenceToNumber $ DNASequence (tail seq')

getFromNode :: DNASequence -> Int
getFromNode (DNASequence seq') = sequenceToNumber $ DNASequence (init seq')
