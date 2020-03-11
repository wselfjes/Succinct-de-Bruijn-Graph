module Types.AssemblyGraphs where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import qualified Data.HashMap  as HM
import qualified Data.Set      as Set
import qualified Data.Vector   as Vec
import           Prelude       hiding (seq)
import           Types.DNA

type Edge = Int

type Node = Int

type Base = Int

data DeBruijnGraph =
  DeBruijnGraph
    { graphBase :: Base
    , bitArr    :: Vec.Vector Bool
    , counts    :: HM.Map Edge Int
    }

instance Show DeBruijnGraph where
  show deBruijnGraph = show multiplicityList
    where
      multiplicityList =
        map
          (\(ind, value) ->
             (numberToSequence (graphBase deBruijnGraph) ind, value))
          edgeCount
      edgeCount = zip [0 ..] (Vec.toList multipliedVec')
      multipliedVec' = multipliedVec deBruijnGraph

instance Eq DeBruijnGraph where
  (==) = (==) `on` (graphBase &&& bitArr &&& counts)

multipliedVec :: DeBruijnGraph -> Vec.Vector Int
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
        value = HM.lookup key (counts deBruijnGraph)

emptyDeBruijn :: Base -> DeBruijnGraph
emptyDeBruijn base =
  DeBruijnGraph
    { graphBase = base
    , bitArr = Vec.generate (4 ^ base) (const False)
    , counts = HM.empty
    }

insertSequence :: DNASequence -> DeBruijnGraph -> DeBruijnGraph
insertSequence dnaseq@(DNASequence seq) deBruijnGraph
  | length seq > graphBase deBruijnGraph =
    insertSequences (splitByN (graphBase deBruijnGraph) dnaseq) deBruijnGraph
  | otherwise = deBruijnGraph {bitArr = newBitArray, counts = counts'}
  where
    setNumber = sequenceToNumber dnaseq
    newBitArray = bitArr deBruijnGraph Vec.// [(setNumber, True)]
    c = counts deBruijnGraph
    counts' =
      if HM.member setNumber c
        then HM.update (Just . (+ 1)) setNumber c
        else HM.insert setNumber 1 c

insertSequences :: [DNASequence] -> DeBruijnGraph -> DeBruijnGraph
insertSequences [] deBruijnGraph = deBruijnGraph
insertSequences (seq:seqs) deBruijnGraph = insertSequences seqs newDeBruijnGraph
  where
    newDeBruijnGraph = insertSequence seq deBruijnGraph

fromDNASequences :: Base -> [DNASequence] -> DeBruijnGraph
fromDNASequences base seqs = insertSequences seqs (emptyDeBruijn base)

diffSequence :: DeBruijnGraph -> DNASequence -> DeBruijnGraph
diffSequence deBruijnGraph seq' =
  deBruijnGraph {bitArr = newBitArr, counts = counts''}
  where
    edge = sequenceToNumber seq'
    (oldCount, counts') =
      HM.updateLookupWithKey (\_ v -> Just (v - 1)) edge (counts deBruijnGraph)
    counts'' =
      case oldCount of
        (Just 1) -> HM.delete edge counts'
        _        -> counts'
    newBitArr =
      case oldCount of
        (Just 1) -> bitArr deBruijnGraph Vec.// [(edge, False)]
        Nothing  -> bitArr deBruijnGraph Vec.// [(edge, False)]
        _        -> bitArr deBruijnGraph

(///) :: DeBruijnGraph -> DNASequence -> DeBruijnGraph
(///) = diffSequence

eulerBackTracking ::
     DeBruijnGraph -> Maybe Edge -> [Edge] -> [DNASequence] -> [DNASequence]
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

eulerPath :: DeBruijnGraph -> Edge -> [Edge] -> [DNASequence] -> [DNASequence]
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

selectStartNode :: [(Node, (Int, Int))] -> Node
selectStartNode l = startNode
  where
    calculatedL =
      map (\(ind, (inEdges, outEdges)) -> (ind, outEdges - inEdges)) l
    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
    startNode =
      if null filteredL
        then fst $ head (filter (\(_, (_, out)) -> out > 0) l)
        else fst $ head filteredL

selectEndNode :: [(Node, (Int, Int))] -> Node
selectEndNode l = endNode
  where
    calculatedL =
      map (\(ind, (inEdges, outEdges)) -> (ind, inEdges - outEdges)) l
    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
    endNode =
      if null filteredL
        then fst $ head (filter (\(_, (in', _)) -> in' > 0) l)
        else fst $ head filteredL

calculateInEdges :: Vec.Vector Int -> Node -> Base -> Int
calculateInEdges arr node base =
  arr Vec.! node + arr Vec.! (node + 4 ^ base) + arr Vec.! (node + 4 ^ base * 2) +
  arr Vec.! (node + 4 ^ base * 3)

calculateOutEdges :: Vec.Vector Int -> Node -> Int
calculateOutEdges arr node =
  arr Vec.! node + arr Vec.! (node + 1) + arr Vec.! (node + 2) +
  arr Vec.! (node + 3)

selectNodes :: Vec.Vector Int -> Base -> (Node, Node)
selectNodes arr base = (selectStartNode inOutArray, selectEndNode inOutArray)
  where
    inOutArray = [inOut i | i <- [0 .. arrSize]]
    arrSize = (4 ^ (base - 1)) - 1
    inOut i =
      (i, (calculateInEdges arr i (base - 1), calculateOutEdges arr (4 * i)))

selectStartEdge :: DeBruijnGraph -> Edge
selectStartEdge deBruijnGraph =
  head $
  successorEdges (bitArr deBruijnGraph) $
  fst (selectNodes multipliedVec' (graphBase deBruijnGraph))
  where
    multipliedVec' = multipliedVec deBruijnGraph

successorEdges :: Vec.Vector Bool -> Node -> [Edge]
successorEdges bitArr' node = s
  where
    ranks =
      filter
        (> rank bitArr' (4 * node - 1))
        [rank bitArr' (4 * node + n) | n <- [0 .. 3]]
    successors = map (select bitArr') ranks
    s = filter (>= 0) $ Set.toList $ Set.fromList successors

select :: Vec.Vector Bool -> Int -> Int
select bitArr' i = select' bitList i 0 - 1
  where
    select' [] _ ind          = ind
    select' (True:_) 0 ind    = ind + 1
    select' (False:_) 0 ind   = ind
    select' (True:xs) i' ind  = select' xs (i' - 1) (ind + 1)
    select' (False:xs) i' ind = select' xs i' (ind + 1)
    bitList = Vec.toList bitArr'

rank :: Vec.Vector Bool -> Int -> Int
rank bitArr' i = sum $ take (i + 1) bitList
  where
    bitList =
      map
        (\x ->
           if x
             then 1
             else 0)
        (Vec.toList bitArr')

assemblyDeBruijn :: DeBruijnGraph -> DNASequence
assemblyDeBruijn deBruijnGraph = mconcat eulerPath'
  where
    eulerPath' = eulerPath deBruijnGraph startEdge [] []
    startEdge = selectStartEdge deBruijnGraph
