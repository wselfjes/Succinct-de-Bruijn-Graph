{-# Language GADTs #-}
module Data.Graph.DeBruijnGraph where

import           Control.Arrow      ((&&&))
import           Data.Function      (on)
import qualified Data.IntMap.Strict as IntMap
import           Prelude            hiding (seq)
import           Data.Sequence.DNA
import           Data.BitArrays.BitArray

type Edge = Int

type Node = Int

type Base = Int

-- | Structure for de Bruijn Graph

data DeBruijnGraph a b = DeBruijnGraph
    { graphBase :: Base -- ^ Length of the graph edge
    , bitArr    :: b -- ^ Bit array for succinct de Bruijn Graph
    , counts    :: IntMap.IntMap Int -- ^ Counts of edges in graph
    }

instance (Show a, Eq a, Enum a) => Show (DeBruijnGraph a b) where
  show = show . toMultiplicityList

toMultiplicityList :: (Enum a) => DeBruijnGraph a b -> [(Sequence a, Int)]
toMultiplicityList (DeBruijnGraph base _ c) = multiplicityList
  where
    multiplicityList =
        map
          (\(ind, value) ->
             (numberToSequence base ind, value))
          edgeCount
    edgeCount = IntMap.toList c

-- | Graph equals if graphBase bitArr and counts are Equal
instance Eq b => Eq (DeBruijnGraph a b) where
  (==) = (==) `on` (graphBase &&& bitArr &&& counts)

-- * Constructions
-- | Create graph without edges
emptyDeBruijn 
  :: (BitArray b)
  => Base -- ^ Length of the edge.
  -> DeBruijnGraph a b -- ^ Empty de Bruijn Graph. Without any edges.
emptyDeBruijn base =
  DeBruijnGraph base (generateEmpty (4 ^ base)) (IntMap.empty)

-- | Create de Bruijn Graph from Sequences
fromSequences
  :: (Enum a, BitArray b)
  => Base -- ^ Length of the edge
  -> [Sequence a] -- ^ Sequeences which will be inserted
  -> DeBruijnGraph a b -- ^ de Bruijn Graph with those sequences
fromSequences base seqs = insertSequences seqs (emptyDeBruijn base)

-- * Operations
-- | Insert sequence into graph.
-- If length of the sequence is greater then base of the graph sequence splited into overlaped chunks.
insertSequence
  :: (Enum a, BitArray b)
  => Sequence a -- ^ Sequence.
  -> DeBruijnGraph a b -- ^ de Bruijn graph.
  -> DeBruijnGraph a b -- ^ de Bruijn graph with sequence.
insertSequence seq@(Sequence l) deBruijnGraph
  | length l > graphBase deBruijnGraph =
    insertSequences (splitByN (graphBase deBruijnGraph) seq) deBruijnGraph
  | otherwise = deBruijnGraph {counts = counts'}
  where
    setNumber = sequenceToNumber seq
    c = counts deBruijnGraph
    counts' =
      if IntMap.member setNumber c
        then IntMap.update (Just . (+ 1)) setNumber c
        else IntMap.insert setNumber 1 c

-- | Insert multiple sequences into de Bruijn graph.
insertSequences
  :: (Enum a, BitArray b)
  => [Sequence a] -- ^ sequences to insert.
  -> DeBruijnGraph a b -- ^ de Bruijn Graph.
  -> DeBruijnGraph a b -- ^ de Bruijn Graph with those sequences.
insertSequences [] deBruijnGraph = deBruijnGraph
insertSequences (seq:seqs) deBruijnGraph = insertSequences seqs newDeBruijnGraph
  where
    newDeBruijnGraph = insertSequence seq deBruijnGraph

preprocess
  :: (Enum a, BitArray b)
  => DeBruijnGraph a b
  -> DeBruijnGraph a b
preprocess deBruijnGraph = deBruijnGraph {bitArr = bitArr'}
  where
    oldBitArr = bitArr deBruijnGraph
    bitArr' = oldBitArr `setBits` zip ((IntMap.keys . counts) deBruijnGraph) (repeat True)

-- | Remove sequence from the graph. If sequence is empty original graph will be returned.
diffSequence
  :: (Enum a, BitArray b)
  => DeBruijnGraph a b -- ^ de Bruijn Graph.
  -> Sequence a -- ^ Sequence which will be removed from de Bruijn Graph.
  -> DeBruijnGraph a b -- ^ de Bruijn Graph without sequence.
diffSequence deBruijnGraph (Sequence []) = deBruijnGraph
diffSequence deBruijnGraph seq' =
  deBruijnGraph {bitArr = newBitArr, counts = counts''}
  where
    edge = sequenceToNumber seq'
    (oldCount, counts') =
      IntMap.updateLookupWithKey (\_ v -> Just (v - 1)) edge (counts deBruijnGraph)
    counts'' =
      case oldCount of
        (Just 1) -> IntMap.delete edge counts'
        _        -> counts'
    newBitArr =
      case oldCount of
        (Just 1) -> bitArr deBruijnGraph `setBits` [(edge, False)]
        Nothing  -> bitArr deBruijnGraph `setBits` [(edge, False)]
        _        -> bitArr deBruijnGraph

-- | Alias for diffSequence
(///)
  :: (Enum a, BitArray b)
  => DeBruijnGraph a b -- ^ de Bruijn Graph.
  -> Sequence a -- ^ Sequence which will be removed from de Bruijn Graph.
  -> DeBruijnGraph a b -- ^ de Bruijn Graph without sequence.
(///) = diffSequence

