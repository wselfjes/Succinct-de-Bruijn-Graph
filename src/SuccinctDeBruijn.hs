module SuccinctDeBruijn where

import           Types.AssemblyGraphs
import           Types.DNA

dnaSequences :: [DNASequence]
dnaSequences =
  map DNASequence [[T, T], [T, C], [C, G], [G, G], [G, A], [A, A], [A, G]]

deBruijnGraph :: DeBruijnGraph
deBruijnGraph = fromDNASequences 2 dnaSequences

run :: IO ()
run = putStrLn "Nothing here, please run tests"
