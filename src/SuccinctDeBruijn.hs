{-# LANGUAGE OverloadedStrings #-}
module SuccinctDeBruijn where

import           Types.AssemblyGraphs
import           Types.DNA
import           Data.Fasta.String.Parse
import           Data.Fasta.String.Types


assembly :: [String] -> DNASequence
assembly fastaSequences = assembledSequence 
  where 
    seqs = map unsafeParseDNASequence fastaSequences
    deBruijnGraph = fromSequences 10 seqs
    assembledSequence = assemblyDeBruijn deBruijnGraph

run :: IO ()
run = do
  fastaData <- readFile "data/sar324_contigs_lane1.fa"
  let parsedFasta = parseFasta fastaData
  let assembledSequence = assembly (map fastaSeq parsedFasta)
  writeFile "data/result.txt" (show assembledSequence)
  putStrLn (show assembledSequence)

