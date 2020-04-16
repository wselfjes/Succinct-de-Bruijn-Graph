{-# LANGUAGE OverloadedStrings #-}
module SuccinctDeBruijn where

import           Types.AssemblyGraphs
import           Types.DNA
import           Data.Fasta.String.Parse
import           Data.Fasta.String.Types
import           System.Environment


assembly :: Base -> [String] -> DNASequence
assembly base fastaSequences = assembledSequence 
  where 
    seqs = map unsafeParseDNASequence fastaSequences
    deBruijnGraph = fromSequences base seqs
    assembledSequence = assemblyDeBruijn deBruijnGraph

run :: IO ()
run = do
  args <- parseArgs
  case args of
    ("", 0)          -> putStrLn "Usage: stack run [base] [file]"
    (fileName, base) -> do
          fastaData <- readFile fileName 
          let parsedFasta = parseFasta fastaData
          let assembledSequence = assembly base (map fastaSeq parsedFasta)
          writeFile "data/result.txt" (show assembledSequence)
          putStrLn (show assembledSequence)



parseArgs :: IO (String, Base)
parseArgs = do
  args <- getArgs
  patternMatching args
  where
    patternMatching :: [String] -> IO (String, Base)
    patternMatching []           = return ("", 0)
    patternMatching [file]       = return (file, 32)
    patternMatching [base, file] = return (file, read base)
    patternMatching _            = return ("", 0)
                                   
