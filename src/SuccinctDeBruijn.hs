{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
module SuccinctDeBruijn where

import           Control.Monad
import           Data.DNA.Assembly
import           Data.Enum.Letter
import           Data.Fasta.String.Parse
import           Data.Fasta.String.Types
import           Data.Proxy
import           GHC.TypeLits
import           Plotting.ColoredDeBruijnGraph
import           System.Environment
import           System.Exit
import           System.IO                      (BufferMode (..), hSetBuffering,
                                                 stdout)
import           Text.Read                      (readMaybe)

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  args <- parseArgs
  case args of
    Nothing               -> putStrLn "Usage: stack run [base] [file]"
    Just (fileName, base) -> case base of
      Nothing -> putStrLn "base must be a natural number"
      Just (SomeNat p@(_ :: Proxy n)) -> runWithArgs p fileName

data AKnownNat (n :: Nat) = forall n. (KnownNat n) => AKnownNat (Proxy n)

runWithArgs :: forall base. KnownNat base => Proxy base -> FilePath -> IO ()
runWithArgs proxy fileName = do
  let baseValue = fromIntegral (natVal proxy)
  putStrLn ("Processing file " <> fileName <> " (using base " <> show baseValue <> ")")
  fastaData <- readFile fileName `as` "Reading file"
  let parsedFasta = parseFasta fastaData
  let readsString = map fastaSeq parsedFasta
  let readsDNASequences = map unsafeLetters readsString
  checkReads baseValue readsDNASequences `as` "Checking reads"
  let deBruijnGraph = graphFromReads readsDNASequences :: DeBruijnGraph base Nucleotide
  writeMultiplicityList deBruijnGraph `as` "Writing as a mulitplicity list"
  drawGraph deBruijnGraph `as` "Drawing deBruijnGraph"
  --let assembledSequence = assemblyDeBruijnUsingEulerianWalk deBruijnGraph
  --writeFile "data/result.txt" (show assembledSequence) `as` "Writing result"

checkReads :: Int -> [ReadSegment] -> IO ()
checkReads k sequences = unless (any ((> k) . length)  sequences) (die "Length of one of the read is less than base")

writeMultiplicityList
  :: (KnownNat n)
  => DeBruijnGraph n (Letter "ACGT")
  -> IO ()
writeMultiplicityList deBruijnGraph = do
  let multiplicityList = toMultiplicityList deBruijnGraph 
  writeFile "data/MultiplicityList.txt" ((unlines . map show) multiplicityList)
  

as :: IO a -> String -> IO a
command `as` name = do
  putStr (name ++ "...")
  x <- command
  putStrLn "[OK]"
  return x

parseArgs :: IO (Maybe (String, Maybe SomeNat))
parseArgs = do
  args <- getArgs
  return $ case args of
    [file]       -> Just (file, someNatVal 32)
    [base, file] | Just intBase <- readMaybe base
      -> Just (file, someNatVal intBase)
    _ -> Nothing
