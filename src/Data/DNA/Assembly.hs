{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.DNA.Assembly where

import           Data.Proxy
import           GHC.TypeLits

import           Data.Enum.FixedList
import           Data.Enum.Letter
import           Data.List.Utils

-- $setup
-- >>> :set -XDataKinds

type Nucleotide = Letter "ACGT"

type ReadSegment = [Nucleotide]

type Contig = [Nucleotide]

type Scaffold = [Maybe Nucleotide]

newtype Chunk n = Chunk { getChunk :: FixedList n Nucleotide }
  deriving (Eq, Ord, Bounded, Enum)

instance Show (Chunk n) where
  show = show . map getLetter . getFixedList . getChunk

-- |
--
-- >>> readChunks (unsafeLetters "AAACGTCAA") :: [Chunk 5]
-- ["AAACG","AACGT","ACGTC","CGTCA","GTCAA"]
readChunks :: forall n. KnownNat n => ReadSegment -> [Chunk n]
readChunks = map (Chunk . FixedList) . chunksOf n
  where
    n = fromIntegral (natVal (Proxy :: Proxy n))
