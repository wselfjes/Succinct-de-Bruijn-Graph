{-# LANGUAGE OverloadedStrings #-}
module SuccinctDeBruijn where

import           Data.Fasta.String.Parse
import           Data.Fasta.String.Types
import           System.Environment
import           Plotting.DeBruijnGraphPlotting
import           Text.Read               (readMaybe)
import           Types.AssemblyGraphs
import           Types.DNA
import           System.Exit
import           System.IO               (BufferMode (..), hSetBuffering,
                                          stdout)


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
  fastaData <- readFile fileName `as` "Reading file"
  let parsedFasta = parseFasta fastaData
  let readsString = (map fastaSeq parsedFasta)
  let readsDNASequences = map unsafeParseDNASequence readsString
  checkReads base readsDNASequences `as` "Checking reads"
  let deBruijnGraph = fromSequences base readsDNASequences :: DeBruijnGraph Nucleotide
  drawGraph deBruijnGraph `as` "Drawing deBruijnGraph"
  let assembledSequence = assemblyDeBruijn deBruijnGraph
  writeFile "data/result.txt" (show assembledSequence) `as` "Writing result"

checkReads :: Base -> [DNASequence] -> IO ()
checkReads k sequences = do 
  if length (filter ((< k) . length . getSequence) sequences) > 0 
  then die "Length of one of the read is less than base" 
  else return ()
    
as :: IO a -> String -> IO a
command `as` name = do
  putStr (name ++ "...")
  x <- command
  putStrLn "[OK]"
  return x

parseArgs :: IO (Maybe (String, Base))
parseArgs = do
  args <- getArgs
  return $ case args of
    [file]       -> Just (file, 32)
    [base, file] | Just intBase <- readMaybe base
      -> Just (file, intBase)
    _ -> Nothing
