module SuccinctDeBruijn where

import           Types.AssemblyGraphs
import           Types.DNA

assembledSequence = assemblyDeBruijn deBruijnGraph

dnaSequences = map DNASequence ["ATGGCGTGCA"]

deBruijnGraph = fromDNASequences 3 dnaSequences

run :: IO ()
run = putStrLn "Nothing here, please run tests"
