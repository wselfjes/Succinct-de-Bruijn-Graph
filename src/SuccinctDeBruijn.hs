module SuccinctDeBruijn where

import           Types.AssemblyGraphs
import           Types.DNA

dnaSequences = map DNASequence ["TT", "TC", "CG", "GG", "GA", "AA", "AG"]

deBruijnGraph = fromDNASequences 2 dnaSequences

run :: IO ()
run = putStrLn "Nothing here, please run tests"
