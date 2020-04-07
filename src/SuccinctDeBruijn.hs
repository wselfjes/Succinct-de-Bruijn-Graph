{-# LANGUAGE OverloadedStrings #-}
module SuccinctDeBruijn where

import           Types.AssemblyGraphs
import           Types.DNA

dnaSequence :: [DNASequence]
dnaSequence = ["TT", "TC", "CG", "GG", "GA", "AA", "AG"]

deBruijnGraph :: DeBruijnGraph Nucleotide
deBruijnGraph = fromSequences 2 dnaSequence

run :: IO ()
run = putStrLn "Nothing here, please run tests"
