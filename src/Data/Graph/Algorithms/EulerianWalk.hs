module Data.Graph.Algorithms.EulerianWalk where

import           Data.RankSelectArray.Class
import           Data.Graph.DeBruijnGraph
import           Data.Sequence.DNA
import qualified Data.Set                 as Set

-- |
eulerBackTracking
  :: (Enum a, RankSelectArray b)
  => DeBruijnGraph a b -- ^ de Bruijn Graph
  -> Maybe Edge -- ^ Starting edge for backtracking. If nothing Starting edge gets from visited edges
  -> [Edge] -- ^ Visited edges
  -> [Sequence a] -- ^ Path
  -> [Sequence a] -- ^ Result Path
eulerBackTracking deBruijnGraph Nothing (newCurrent:xs) path =
  eulerBackTracking deBruijnGraph (Just newCurrent) xs path
eulerBackTracking _ Nothing [] path = path
eulerBackTracking deBruijnGraph (Just current) [] path =
  case successors of
    (_:_) -> eulerPath deBruijnGraph successor [] newPath
    _     -> newPath
  where
    successors =
      (successorEdges (bitArr deBruijnGraph) .
       getFromNode . numberToSequence (graphBase deBruijnGraph))
        current
    successor = head successors
    newPath = numberToSequence (graphBase deBruijnGraph) current : path
eulerBackTracking deBruijnGraph (Just current) visited@(newCurrent:xs) path =
  case successors of
    (_:_) -> eulerPath deBruijnGraph successor visited newPath
    _     -> eulerBackTracking deBruijnGraph (Just newCurrent) xs newPath
  where
    successors =
      (successorEdges (bitArr deBruijnGraph) .
       getFromNode . numberToSequence (graphBase deBruijnGraph))
        current
    successor = head successors
    newPath = numberToSequence (graphBase deBruijnGraph) current : path

-- | Find Euler Path in de Bruijn Graph. An Euler path is a path that uses every edge of a graph exactly once.
eulerPath
  :: (Enum a, RankSelectArray b)
  => DeBruijnGraph a b -- ^ de Bruijn Graph
  -> Edge -- ^ Starting edge
  -> [Edge] -- ^ Visited edges
  -> [Sequence a] -- ^ Path
  -> [Sequence a] -- ^ Result path
eulerPath deBruijnGraph current visited path =
  case successors of
    (_:_) -> eulerPath newDeBruijnGraph newCurrent newVisited path
    _     -> eulerBackTracking newDeBruijnGraph Nothing newVisited path
  where
    successors =
      (successorEdges (bitArr newDeBruijnGraph) .
       getToNode . numberToSequence (graphBase newDeBruijnGraph))
        current
    newDeBruijnGraph =
      deBruijnGraph /// numberToSequence (graphBase deBruijnGraph) current
    successor = head successors
    newVisited = current : visited
    newCurrent = successor

selectStartNode ::
     [(Node, (Int, Int))] -- ^ List of nodes with count of in edges, and out edges
  -> Node -- ^ Start node
selectStartNode l = startNode
  where
    calculatedL =
      map (\(ind, (inEdges, outEdges)) -> (ind, outEdges - inEdges)) l
    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
    startNode =
      if null filteredL
        then fst $ head (filter (\(_, (_, out)) -> out > 0) l)
        else fst $ head filteredL

-- | Select end node.
selectEndNode ::
     [(Node, (Int, Int))] -- ^ List of nodes with count of in edges, and out edges
  -> Node -- ^ End node
selectEndNode l = endNode
  where
    calculatedL =
      map (\(ind, (inEdges, outEdges)) -> (ind, inEdges - outEdges)) l
    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
    endNode =
      if null filteredL
        then fst $ head (filter (\(_, (in', _)) -> in' > 0) l)
        else fst $ head filteredL

---- | Calculate count of edges which ends at node.
--calculateInEdges ::
--     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
--  -> Node -- ^ Node for which we need to find in edges.
--  -> Base -- ^ Length of the edge in graph
--  -> Int -- ^ Number of edges that ends at node
--calculateInEdges arr node base =
--  arr Vec.! node + arr Vec.! (node + 4 ^ base) + arr Vec.! (node + 4 ^ base * 2) +
--  arr Vec.! (node + 4 ^ base * 3)
--
---- | Calculate count of edges which starts at node.
--calculateOutEdges ::
--     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
--  -> Node -- ^ Node for which we need to find out edges.
--  -> Int -- ^ Number of edges that starts at node.
--calculateOutEdges arr node =
--  arr Vec.! node + arr Vec.! (node + 1) + arr Vec.! (node + 2) +
--  arr Vec.! (node + 3)
--
---- | Select first and last nodes for euler path
--selectNodes ::
--     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
--  -> Base -- ^ Length of the edge in graph
--  -> (Node, Node) -- ^ (Start node, End node)
--selectNodes arr base = (selectStartNode inOutArray, selectEndNode inOutArray)
--  where
--    inOutArray = [inOut i | i <- [0 .. arrSize]]
--    arrSize = (4 ^ (base - 1)) - 1
--    inOut i =
--      (i, (calculateInEdges arr i (base - 1), calculateOutEdges arr (4 * i)))
--
---- | Select start edge for euler path
--selectStartEdge ::
--     DeBruijnGraph a b -- ^ de Bruijn Graph
--  -> Edge -- ^ Start edge for euler path
--selectStartEdge deBruijnGraph =
--  head $
--  successorEdges (bitArr deBruijnGraph) $
--  fst (selectNodes multipliedVec' (graphBase deBruijnGraph))
--  where
--    multipliedVec' = multipliedVec deBruijnGraph

-- | Successor edges of node
successorEdges
  :: (RankSelectArray b)
  => b -- ^ Bit array
  -> Node -- ^ Node
  -> [Edge] -- ^ List of edges from node
successorEdges bitArr' node = s
  where
    ranks =
      filter
        (> rank bitArr' True (4 * node - 1))
        [rank bitArr' True (4 * node + n) | n <- [0 .. 3]]
    successors = map (select bitArr' True) ranks
    s = filter (>= 0) $ Set.toList $ Set.fromList successors


-- | Assembly de Bruijn Graph. Create original DNA from sequencing subDNAs
assemblyDeBruijnUsingEulerianWalk
  :: (RankSelectArray b)
  => DeBruijnGraph Nucleotide b-- ^ de Bruijn Graph
  -> DNASequence                             -- ^ Assembled DNA
assemblyDeBruijnUsingEulerianWalk deBruijnGraph = foldl (mergeDNASequence) (Sequence []) eulerPath'
  where
    eulerPath' = eulerPath deBruijnGraph startEdge [] []
    startEdge = 0 --selectStartEdge deBruijnGraph
