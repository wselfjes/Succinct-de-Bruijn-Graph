module Types.AssemblyGraphs where

import           Data.Array
import           Data.BitArray
import qualified Data.Set      as Set
import           Prelude       hiding (seq)
import           Types.DNA

data DeBruijnGraph =
  DeBruijnGraph Int BitArray [(Int, Bool)] (Array Int Int)

instance Show DeBruijnGraph where
  show (DeBruijnGraph p _ _ arr) = show multiplicityList
    where
      multiplicityList =
        map (\(ind, value) -> (numberToSequence p ind, value)) (assocs arr)

instance Eq DeBruijnGraph where
  (==) (DeBruijnGraph p1 bitArr1 _ arr1) (DeBruijnGraph p2 bitArr2 _ arr2) =
    p1 == p2 && arr1 == arr2 && bitArr1 == bitArr2

emptyDeBruijn :: Int -> DeBruijnGraph
emptyDeBruijn p =
  DeBruijnGraph
    p
    (bitArray (0, 4 ^ p - 1) [])
    []
    (array (0, 4 ^ p - 1) [(i, 0) | i <- [0 .. 4 ^ p - 1]])

insertSequence :: DNASequence -> DeBruijnGraph -> DeBruijnGraph
insertSequence seq (DeBruijnGraph p bitArr inserted arr)
  | length (show seq) > p =
    insertSequences (splitByN p seq) (DeBruijnGraph p bitArr inserted arr)
  | otherwise = DeBruijnGraph p newBitArray newInserted newArr
  where
    setNumber = sequenceToNumber seq
    newInserted = (setNumber, True) : inserted
    newBitArray = bitArray (bitArrayBounds bitArr) newInserted
    occures = (arr ! setNumber) + 1
    newArr = arr // [(setNumber, occures)]

insertSequences :: [DNASequence] -> DeBruijnGraph -> DeBruijnGraph
insertSequences [] deBruijnGraph = deBruijnGraph
insertSequences (seq:seqs) deBruijnGraph = insertSequences seqs newDeBruijnGraph
  where
    newDeBruijnGraph = insertSequence seq deBruijnGraph

fromDNASequences :: Int -> [DNASequence] -> DeBruijnGraph
fromDNASequences p seqs = insertSequences seqs (emptyDeBruijn p)

(///) :: DeBruijnGraph -> DNASequence -> DeBruijnGraph
(///) (DeBruijnGraph p _ includes occurrences) seq' =
  DeBruijnGraph p newBitArr newIncludes newOccurrences
  where
    edge = sequenceToNumber seq'
    occurrence = occurrences ! edge
    newOccurrences = occurrences // [(edge, occurrence - 1)]
    newBitArr = bitArray (0, 4 ^ p - 1) newIncludes
    newIncludes =
      if newOccurrences ! edge == 0
        then filter (\(ind, _) -> ind /= edge) includes
        else includes

eulerBackTracking ::
     DeBruijnGraph -> Int -> [Int] -> [DNASequence] -> [DNASequence]
eulerBackTracking deBruijnGraph (-1) (newCurrent:xs) path =
  eulerBackTracking deBruijnGraph newCurrent xs path
eulerBackTracking deBruijnGraph@(DeBruijnGraph p bitArr _ _) current [] path
  | null successors = newPath
  | otherwise = eulerPath deBruijnGraph successor [] newPath
  where
    successors =
      (successorEdges bitArr . getFromNode . numberToSequence p) current
    successor = head successors
    newPath = numberToSequence p current : path
eulerBackTracking deBruijnGraph@(DeBruijnGraph p bitArr _ _) current (newCurrent:xs) path
  | null successors = eulerBackTracking deBruijnGraph newCurrent xs newPath
  | otherwise = eulerPath deBruijnGraph successor xs newPath
  where
    successors =
      (successorEdges bitArr . getFromNode . numberToSequence p) current
    successor = head successors
    newPath = numberToSequence p current : path

eulerPath :: DeBruijnGraph -> Int -> [Int] -> [DNASequence] -> [DNASequence]
eulerPath deBruijnGraph@(DeBruijnGraph p _ _ _) current visited path
  | null successors = eulerBackTracking newDeBruijnGraph (-1) newVisited path --map (numberToSequence p) newVisited
  | otherwise = eulerPath newDeBruijnGraph newCurrent newVisited path
  where
    successors = successorEdges bitArr' $ getToNode $ numberToSequence p current
    newDeBruijnGraph@(DeBruijnGraph _ bitArr' _ _) =
      deBruijnGraph /// numberToSequence p current
    successor = head successors
    newVisited = current : visited
    newCurrent = successor

selectStartNode :: [(Int, (Int, Int))] -> Int
selectStartNode l = startNode
  where
    calculatedL =
      map (\(ind, (inEdges, outEdges)) -> (ind, outEdges - inEdges)) l
    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
    startNode =
      if null filteredL
        then -1
        else fst $ head filteredL

selectEndNode :: [(Int, (Int, Int))] -> Int
selectEndNode l = endNode
  where
    calculatedL =
      map (\(ind, (inEdges, outEdges)) -> (ind, inEdges - outEdges)) l
    filteredL = filter (\(_, diff) -> diff == 1) calculatedL
    endNode = fst $ head filteredL

calculateInEdges :: Array Int Int -> Int -> Int -> Int
calculateInEdges arr node p =
  arr ! node + arr ! (node + 4 ^ p) + arr ! (node + 4 ^ p * 2) +
  arr ! (node + 4 ^ p * 3)

calculateOutEdges :: Array Int Int -> Int -> Int
calculateOutEdges arr node =
  arr ! node + arr ! (node + 1) + arr ! (node + 2) + arr ! (node + 3)

selectNodes :: Array Int Int -> Int -> (Int, Int)
selectNodes arr p =
  (selectStartNode $ assocs inOutArray, selectEndNode $ assocs inOutArray)
  where
    inOutArray = array (0, arrSize) [inOut i | i <- [0 .. arrSize]]
    arrSize = (4 ^ (p - 1)) - 1
    inOut i =
      (i, (calculateInEdges arr i (p - 1), calculateOutEdges arr (4 * i)))

selectStartEdge :: DeBruijnGraph -> Int
selectStartEdge (DeBruijnGraph p bitArr _ occurrences) =
  head $ successorEdges bitArr $ fst (selectNodes occurrences p)

successorEdges :: BitArray -> Int -> [Int]
successorEdges bitArr node = s
  where
    ranks =
      filter
        (> rank bitArr (4 * node - 1))
        [rank bitArr (4 * node + n) | n <- [0 .. 3]]
    successors = map (select bitArr) ranks
    s = filter (>= 0) $ Set.toList $ Set.fromList successors

select :: BitArray -> Int -> Int
select bitArr i = select' bitList i 0 - 1
  where
    select' [] _ ind          = ind
    select' (True:_) 0 ind    = ind + 1
    select' (False:_) 0 ind   = ind
    select' (True:xs) i' ind  = select' xs (i' - 1) (ind + 1)
    select' (False:xs) i' ind = select' xs i' (ind + 1)
    bitList = bits bitArr

rank :: BitArray -> Int -> Int
rank bitArr i = sum $ take (i + 1) bitList
  where
    bitList = bits01 bitArr

assemblyDeBruijn :: DeBruijnGraph -> DNASequence
assemblyDeBruijn deBruijnGraph = foldl (+++) (DNASequence "") eulerPath'
  where
    eulerPath' = eulerPath deBruijnGraph startEdge [] []
    startEdge = selectStartEdge deBruijnGraph
