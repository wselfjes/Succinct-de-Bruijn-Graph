{-# LANGUAGE OverloadedStrings #-}
module SuccinctDeBruijn where

import           Data.Fasta.String.Parse
import           Data.Fasta.String.Types
import           System.Environment
import           Text.Read               (readMaybe)
import           Types.AssemblyGraphs
import           Types.DNA

import           System.IO               (BufferMode (..), hSetBuffering,
                                          stdout)

assembly :: Base -> [String] -> DNASequence
assembly base fastaSequences = assembledSequence
  where
    seqs = map unsafeParseDNASequence fastaSequences
    deBruijnGraph = fromSequences base seqs
    assembledSequence = assemblyDeBruijn deBruijnGraph

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  args <- parseArgs
  case args of
    Nothing               -> putStrLn "Usage: stack run [base] [file]"
    Just (fileName, base) -> runWithArgs base fileName

runWithArgs :: Base -> FilePath -> IO ()
runWithArgs base fileName = do
  putStrLn ("Processing file " <> fileName <> " (using base " <> show base <> ")")
  fastaData <- readFile fileName  `as` "Reading file"
  let parsedFasta = parseFasta fastaData
  print (length parsedFasta) `as` "Number of input sequences"
  let assembledSequence = assembly base (map fastaSeq parsedFasta)
  writeFile "data/result.txt" (show assembledSequence) `as` "Writing result"
  putStrLn (show assembledSequence)

as :: IO a -> String -> IO a
command `as` name = do
  putStr (name ++ "... ")
  x <- command
  putStrLn " [OK]"
  return x

parseArgs :: IO (Maybe (String, Base))
parseArgs = do
  args <- getArgs
  return $ case args of
    [file]       -> Just (file, 32)
    [base, file] | Just intBase <- readMaybe base
      -> Just (file, intBase)
    _ -> Nothing
