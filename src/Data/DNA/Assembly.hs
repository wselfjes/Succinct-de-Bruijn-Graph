{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.DNA.Assembly where

import           Data.List           (nub)
import           Data.Proxy
import           GHC.TypeLits

import           Data.RankSelect.Map (RankSelectMap)
import qualified Data.RankSelect.Map as RSMap

import           Data.Enum.FixedList
import           Data.Enum.Letter
import           Data.Enum.Utils     (boundedEnumSize, toBoundedEnum)
import           Data.List.Utils     (chunksOf, nubSort)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications

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

newtype DeBruijnGraph n a = DeBruijnGraph
  { edgeCount :: RankSelectMap (Edge n a) Int }

instance (Show (Edge n a), Bounded a, Enum a, KnownNat (n + 1))
  => Show (DeBruijnGraph n a) where
  show = show . toMultiplicityList

toMultiplicityList
  :: (Bounded a, Enum a, KnownNat (n + 1))
  => DeBruijnGraph n a -> [(Edge n a, Int)]
toMultiplicityList = RSMap.toListBoundedEnum . edgeCount

type Edge n = FixedList (n + 1)
type Node n = FixedList n

edgeNodes :: Edge n a -> [Node n a]
edgeNodes (FixedList xs) = [FixedList (init xs), FixedList (tail xs)]

-- |
-- >>> edges (graphFromReads @2 [unsafeLetters @"ACTG" "AAACCAACC"])
-- ["AAA","CAA","CCA","AAC","ACC"]
edges :: (Bounded a, Enum a, KnownNat (n + 1)) => DeBruijnGraph n a -> [Edge n a]
edges = RSMap.keys toBoundedEnum . edgeCount

-- |
--
-- >>> nodes (graphFromReads @2 [unsafeLetters @"ACTG" "AAACCAACC"])
-- ["AA","CA","CC","AC"]
nodes
  :: (Bounded a, Enum a, Eq a, KnownNat n, KnownNat (n + 1))
  => DeBruijnGraph n a -> [Node n a]
nodes = nub . concatMap edgeNodes . edges

-- |
--
-- >>> graphFromReads @2 [unsafeLetters @"ACTG" "AAACCAACC"]
-- [("AAA",1),("CAA",1),("CCA",1),("AAC",2),("ACC",2)]
-- >>> (Data.RankSelectArray.SDArray.toOnes . Data.RankSelect.Map.rsBitmap . edgeCount) (graphFromReads @1 [unsafeLetters @"ACGT" "TTCGGAAG"])
-- [0,2,6,8,10,13,15]
graphFromReads
  :: forall n a. (KnownNat (n + 1), Bounded a, Enum a, Show a)
  => [[a]] -> DeBruijnGraph n a
graphFromReads segments = DeBruijnGraph $ RSMap.fromEnumListWith (+) size
  [ (chunkId, 1)
  | segment <- segments
  , chunkId <- (fixedBoundedEnumChunks (Proxy @(n + 1)) segment)
  ] where size = boundedEnumSize (Proxy @(Edge n a))

-- | Successor edges of a node.
--
-- >>> Data.DNA.Assembly.successorEdges (graphFromReads @2 [unsafeLetters @"AB" "AABABBA"]) "AB"
-- [("ABA",1),("ABB",1)]
successorEdges
  :: forall n a. (Bounded a, Enum a, KnownNat n, KnownNat (n + 1))
  => DeBruijnGraph n a
  -> Node n a -- ^ Node
  -> [(Edge n a, Int)] -- ^ List of edges from node
successorEdges (DeBruijnGraph graph) node =
  [ (toBoundedEnum edgeId, count)
  | (edgeId, count) <- nubSort successors
  , edgeId >= 0
  , count > 0
  ]
  where
    ranks =
      filter (> RSMap.rankEnum (nodeVal - 1) graph)
        [RSMap.rankEnum (nodeVal + i) graph | i <- [0 .. (base - 1)]]
    successors = map (`RSMap.selectEnum` graph) ranks
    base = boundedEnumSize (Proxy @a)
    nodeVal = base * fromEnum node

-- | Mark edge visited
markEdge
  :: forall n a. (Bounded a, Enum a, KnownNat n, KnownNat (n + 1))
  => DeBruijnGraph n a -- ^ Original de Bruijn Graph
  -> Edge n a          -- ^ Edge, which will be marked
  -> DeBruijnGraph n a -- ^ New de Bruijn Graph
markEdge (DeBruijnGraph graph) edge = DeBruijnGraph graph'
  where
    graph' = RSMap.update (subtract 1) edge graph

