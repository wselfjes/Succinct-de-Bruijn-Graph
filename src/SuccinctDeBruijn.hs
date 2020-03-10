module SuccinctDeBruijn where

import           Types.AssemblyGraphs
import           Types.DNA

assembledSequence :: DNASequence
assembledSequence = assemblyDeBruijn deBruijnGraph

dnaSequences :: [DNASequence]
dnaSequences = map DNASequence ["ATGGCGTGCA"]

deBruijnGraph :: DeBruijnGraph
deBruijnGraph = fromDNASequences 3 dnaSequences

run :: IO ()
run = putStrLn "Nothing here, please run tests"
