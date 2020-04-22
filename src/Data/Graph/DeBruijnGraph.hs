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

---- |
--eulerBackTracking
--  :: (Enum a, BitArray b)
--  => DeBruijnGraph a b -- ^ de Bruijn Graph
--  -> Maybe Edge -- ^ Starting edge for backtracking. If nothing Starting edge gets from visited edges
--  -> [Edge] -- ^ Visited edges
--  -> [Sequence a] -- ^ Path
--  -> [Sequence a] -- ^ Result Path
--eulerBackTracking deBruijnGraph Nothing (newCurrent:xs) path =
--  eulerBackTracking deBruijnGraph (Just newCurrent) xs path
--eulerBackTracking _ Nothing [] path = path
--eulerBackTracking deBruijnGraph (Just current) [] path =
--  case successors of
--    (_:_) -> eulerPath deBruijnGraph successor [] newPath
--    _     -> newPath
--  where
--    successors =
--      (successorEdges (bitArr deBruijnGraph) .
--       getFromNode . numberToSequence (graphBase deBruijnGraph))
--        current
--    successor = head successors
--    newPath = numberToSequence (graphBase deBruijnGraph) current : path
--eulerBackTracking deBruijnGraph (Just current) visited@(newCurrent:xs) path =
--  case successors of
--    (_:_) -> eulerPath deBruijnGraph successor visited newPath
--    _     -> eulerBackTracking deBruijnGraph (Just newCurrent) xs newPath
--  where
--    successors =
--      (successorEdges (bitArr deBruijnGraph) .
--       getFromNode . numberToSequence (graphBase deBruijnGraph))
--        current
--    successor = head successors
--    newPath = numberToSequence (graphBase deBruijnGraph) current : path
--
---- | Find Euler Path in de Bruijn Graph. An Euler path is a path that uses every edge of a graph exactly once.
--eulerPath
--  :: (Enum a, BitArray b)
--  => DeBruijnGraph a b -- ^ de Bruijn Graph
--  -> Edge -- ^ Starting edge
--  -> [Edge] -- ^ Visited edges
--  -> [Sequence a] -- ^ Path
--  -> [Sequence a] -- ^ Result path
--eulerPath deBruijnGraph current visited path =
--  case successors of
--    (_:_) -> eulerPath newDeBruijnGraph newCurrent newVisited path
--    _     -> eulerBackTracking newDeBruijnGraph Nothing newVisited path
--  where
--    successors =
--      (successorEdges (bitArr newDeBruijnGraph) .
--       getToNode . numberToSequence (graphBase newDeBruijnGraph))
--        current
--    newDeBruijnGraph =
--      deBruijnGraph /// numberToSequence (graphBase deBruijnGraph) current
--    successor = head successors
--    newVisited = current : visited
--    newCurrent = successor
--
--selectStartNode ::
--     [(Node, (Int, Int))] -- ^ List of nodes with count of in edges, and out edges
--  -> Node -- ^ Start node
--selectStartNode l = startNode
--  where
--    calculatedL =
--      map (\(ind, (inEdges, outEdges)) -> (ind, outEdges - inEdges)) l
--    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
--    startNode =
--      if null filteredL
--        then fst $ head (filter (\(_, (_, out)) -> out > 0) l)
--        else fst $ head filteredL
--
---- | Select end node.
--selectEndNode ::
--     [(Node, (Int, Int))] -- ^ List of nodes with count of in edges, and out edges
--  -> Node -- ^ End node
--selectEndNode l = endNode
--  where
--    calculatedL =
--      map (\(ind, (inEdges, outEdges)) -> (ind, inEdges - outEdges)) l
--    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
--    endNode =
--      if null filteredL
--        then fst $ head (filter (\(_, (in', _)) -> in' > 0) l)
--        else fst $ head filteredL
--
------ | Calculate count of edges which ends at node.
----calculateInEdges ::
----     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
----  -> Node -- ^ Node for which we need to find in edges.
----  -> Base -- ^ Length of the edge in graph
----  -> Int -- ^ Number of edges that ends at node
----calculateInEdges arr node base =
----  arr Vec.! node + arr Vec.! (node + 4 ^ base) + arr Vec.! (node + 4 ^ base * 2) +
----  arr Vec.! (node + 4 ^ base * 3)
----
------ | Calculate count of edges which starts at node.
----calculateOutEdges ::
----     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
----  -> Node -- ^ Node for which we need to find out edges.
----  -> Int -- ^ Number of edges that starts at node.
----calculateOutEdges arr node =
----  arr Vec.! node + arr Vec.! (node + 1) + arr Vec.! (node + 2) +
----  arr Vec.! (node + 3)
----
------ | Select first and last nodes for euler path
----selectNodes ::
----     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
----  -> Base -- ^ Length of the edge in graph
----  -> (Node, Node) -- ^ (Start node, End node)
----selectNodes arr base = (selectStartNode inOutArray, selectEndNode inOutArray)
----  where
----    inOutArray = [inOut i | i <- [0 .. arrSize]]
----    arrSize = (4 ^ (base - 1)) - 1
----    inOut i =
----      (i, (calculateInEdges arr i (base - 1), calculateOutEdges arr (4 * i)))
----
------ | Select start edge for euler path
----selectStartEdge ::
----     DeBruijnGraph a b -- ^ de Bruijn Graph
----  -> Edge -- ^ Start edge for euler path
----selectStartEdge deBruijnGraph =
----  head $
----  successorEdges (bitArr deBruijnGraph) $
----  fst (selectNodes multipliedVec' (graphBase deBruijnGraph))
----  where
----    multipliedVec' = multipliedVec deBruijnGraph
--
---- | Successor edges of node
--successorEdges 
--  :: (BitArray b)
--  => b -- ^ Bit array
--  -> Node -- ^ Node
--  -> [Edge] -- ^ List of edges from node
--successorEdges bitArr' node = s
--  where
--    ranks =
--      filter
--        (> rank bitArr' (4 * node - 1))
--        [rank bitArr' (4 * node + n) | n <- [0 .. 3]]
--    successors = map (select bitArr') ranks
--    s = filter (>= 0) $ Set.toList $ Set.fromList successors
--
--
---- | Assembly de Bruijn Graph. Create original DNA from sequencing subDNAs
--assemblyDeBruijn
--  :: DeBruijnGraph Nucleotide VectorBitArray -- ^ de Bruijn Graph
--  -> DNASequence                             -- ^ Assembled DNA
--assemblyDeBruijn deBruijnGraph = foldl (mergeDNASequence) (Sequence []) eulerPath'
--  where
--    eulerPath' = eulerPath deBruijnGraph startEdge [] []
--    startEdge = 0 --selectStartEdge deBruijnGraph
