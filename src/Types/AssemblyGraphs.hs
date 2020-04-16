module Types.AssemblyGraphs where

import           Control.Arrow      ((&&&))
import           Data.Function      (on)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set           as Set
import qualified Data.Vector        as Vec
import           Prelude            hiding (seq)
import           Types.DNA

type Edge = Int

type Node = Int

type Base = Int

-- | Structure for de Bruijn Graph
data DeBruijnGraph alphabet = DeBruijnGraph
    { graphBase :: Base -- ^ Length of the graph edge
    , bitArr    :: Vec.Vector Bool -- ^ Bit array for succinct de Bruijn Graph
    , counts    :: IntMap.IntMap Int -- ^ Counts of edges in graph
    }

instance (Show a, Eq a, Enum a) => Show (DeBruijnGraph a) where
  show = show . toMultiplicityList

toMultiplicityList :: (Enum a) => DeBruijnGraph a -> [(Sequence a, Int)]
toMultiplicityList deBruijnGraph = multiplicityList
  where
    multiplicityList =
        map
          (\(ind, value) ->
             (numberToSequence (graphBase deBruijnGraph) ind, value))
          edgeCount
    edgeCount = zip [0 ..] (Vec.toList multipliedVec')
    multipliedVec' = multipliedVec deBruijnGraph

-- | Graph equals if graphBase bitArr and counts are Equal
instance Eq (DeBruijnGraph a) where
  (==) = (==) `on` (graphBase &&& bitArr &&& counts)

-- | Get edges with counts
multipliedVec ::
     DeBruijnGraph a -- ^ de Bruijn Graph
  -> Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
multipliedVec deBruijnGraph =
  Vec.imap
    (\i x ->
       if x
         then lookup' i
         else 0)
    (bitArr deBruijnGraph)
  where
    lookup' key =
      case value of
        (Just v) -> v
        Nothing  -> 1
      where
        value = IntMap.lookup key (counts deBruijnGraph)

-- * Constructions
-- | >>> show (emptyDeBruijn 2)
-- "[(AA,0),(AC,0),(AG,0),(AT,0),(CA,0),(CC,0),(CG,0),(CT,0),(GA,0),(GC,0),(GG,0),(GT,0),(TA,0),(TC,0),(TG,0),(TT,0)]"
emptyDeBruijn ::
     Base -- ^ Length of the edge.
  -> DeBruijnGraph a -- ^ Empty de Bruijn Graph. Without any edge.
emptyDeBruijn base =
  DeBruijnGraph
    { graphBase = base
    , bitArr = Vec.generate (4 ^ base) (const False)
    , counts = IntMap.empty
    }

-- | Create de Bruijn Graph from Sequences
fromSequences
  :: (Enum a)
  => Base -- ^ Length of the edge
  -> [Sequence a] -- ^ Sequeences which will be inserted
  -> DeBruijnGraph a -- ^ de Bruijn Graph with those sequences
fromSequences base seqs = insertSequences seqs (emptyDeBruijn base)

-- * Operations
-- | Insert sequence into graph.
-- If length of the sequence is greater then base of the graph sequence splited into overlaped chunks.
insertSequence
  :: (Enum a)
  => Sequence a -- ^ Sequence.
  -> DeBruijnGraph a -- ^ de Bruijn graph.
  -> DeBruijnGraph a -- ^ de Bruijn graph with sequence.
insertSequence seq@(Sequence l) deBruijnGraph
  | length l > graphBase deBruijnGraph =
    insertSequences (splitByN (graphBase deBruijnGraph) seq) deBruijnGraph
  | otherwise = deBruijnGraph {bitArr = newBitArray, counts = counts'}
  where
    setNumber = sequenceToNumber seq
    newBitArray = bitArr deBruijnGraph Vec.// [(setNumber, True)]
    c = counts deBruijnGraph
    counts' =
      if IntMap.member setNumber c
        then IntMap.update (Just . (+ 1)) setNumber c
        else IntMap.insert setNumber 1 c

-- | Insert multiple sequences into de Bruijn graph.
insertSequences
  :: (Enum a)
  => [Sequence a] -- ^ sequences to insert.
  -> DeBruijnGraph a -- ^ de Bruijn Graph.
  -> DeBruijnGraph a -- ^ de Bruijn Graph with those sequences.
insertSequences [] deBruijnGraph = deBruijnGraph
insertSequences (seq:seqs) deBruijnGraph = insertSequences seqs newDeBruijnGraph
  where
    newDeBruijnGraph = insertSequence seq deBruijnGraph

-- | Remove sequence from the graph. If sequence is empty original graph will be returned.
diffSequence
  :: (Enum a)
  => DeBruijnGraph a -- ^ de Bruijn Graph.
  -> Sequence a -- ^ Sequence which will be removed from de Bruijn Graph.
  -> DeBruijnGraph a -- ^ de Bruijn Graph without sequence.
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
        (Just 1) -> bitArr deBruijnGraph Vec.// [(edge, False)]
        Nothing  -> bitArr deBruijnGraph Vec.// [(edge, False)]
        _        -> bitArr deBruijnGraph

-- | Alias for diffSequence
(///)
  :: (Enum a)
  => DeBruijnGraph a -- ^ de Bruijn Graph.
  -> Sequence a -- ^ Sequence which will be removed from de Bruijn Graph.
  -> DeBruijnGraph a -- ^ de Bruijn Graph without sequence.
(///) = diffSequence

-- |
eulerBackTracking
  :: (Enum a)
  => DeBruijnGraph a -- ^ de Bruijn Graph
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
  :: (Enum a)
  => DeBruijnGraph a -- ^ de Bruijn Graph
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

-- | Calculate count of edges which ends at node.
calculateInEdges ::
     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
  -> Node -- ^ Node for which we need to find in edges.
  -> Base -- ^ Length of the edge in graph
  -> Int -- ^ Number of edges that ends at node
calculateInEdges arr node base =
  arr Vec.! node + arr Vec.! (node + 4 ^ base) + arr Vec.! (node + 4 ^ base * 2) +
  arr Vec.! (node + 4 ^ base * 3)

-- | Calculate count of edges which starts at node.
calculateOutEdges ::
     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
  -> Node -- ^ Node for which we need to find out edges.
  -> Int -- ^ Number of edges that starts at node.
calculateOutEdges arr node =
  arr Vec.! node + arr Vec.! (node + 1) + arr Vec.! (node + 2) +
  arr Vec.! (node + 3)

-- | Select first and last nodes for euler path
selectNodes ::
     Vec.Vector Int -- ^ Vector where index is number of edge and value is number of occurences in graph.
  -> Base -- ^ Length of the edge in graph
  -> (Node, Node) -- ^ (Start node, End node)
selectNodes arr base = (selectStartNode inOutArray, selectEndNode inOutArray)
  where
    inOutArray = [inOut i | i <- [0 .. arrSize]]
    arrSize = (4 ^ (base - 1)) - 1
    inOut i =
      (i, (calculateInEdges arr i (base - 1), calculateOutEdges arr (4 * i)))

-- | Select start edge for euler path
selectStartEdge ::
     DeBruijnGraph a -- ^ de Bruijn Graph
  -> Edge -- ^ Start edge for euler path
selectStartEdge deBruijnGraph =
  head $
  successorEdges (bitArr deBruijnGraph) $
  fst (selectNodes multipliedVec' (graphBase deBruijnGraph))
  where
    multipliedVec' = multipliedVec deBruijnGraph

-- | Successor edges of node
successorEdges ::
     Vec.Vector Bool -- ^ Bit array
  -> Node -- ^ Node
  -> [Edge] -- ^ List of edges from node
successorEdges bitArr' node = s
  where
    ranks =
      filter
        (> rank bitArr' (4 * node - 1))
        [rank bitArr' (4 * node + n) | n <- [0 .. 3]]
    successors = map (select bitArr') ranks
    s = filter (>= 0) $ Set.toList $ Set.fromList successors

-- | Returns the position of the i-th occurrence of 1
select ::
     Vec.Vector Bool -- ^ Bit array
  -> Int -- ^ i-th occurrence of 1
  -> Int -- ^ Position of i-th occurrence of 1 in bit array
select bitArr' i = select' bitList i 0 - 1
  where
    select' [] _ ind          = ind
    select' (True:_) 0 ind    = ind + 1
    select' (False:_) 0 ind   = ind
    select' (True:xs) i' ind  = select' xs (i' - 1) (ind + 1)
    select' (False:xs) i' ind = select' xs i' (ind + 1)
    bitList = Vec.toList bitArr'

-- | Returns the number of elements equal to 1 up to position i
rank ::
     Vec.Vector Bool -- ^ Bit array
  -> Int -- ^ Position i in bit Array
  -> Int -- ^ Number of ones up to position i
rank bitArr' i = sum $ take (i + 1) bitList
  where
    bitList =
      map
        (\x ->
           if x
             then 1
             else 0)
        (Vec.toList bitArr')

-- | Assembly de Bruijn Graph. Create original DNA from sequencing subDNAs
assemblyDeBruijn
  :: DeBruijnGraph Nucleotide -- ^ de Bruijn Graph
  -> DNASequence -- ^ Assembled DNA
assemblyDeBruijn deBruijnGraph = foldl (mergeDNASequence) (Sequence []) eulerPath'
  where
    eulerPath' = eulerPath deBruijnGraph startEdge [] []
    startEdge = selectStartEdge deBruijnGraph
