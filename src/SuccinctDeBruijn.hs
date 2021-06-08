{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module SuccinctDeBruijn where

import           Data.List                    (intercalate)
import           System.Environment
import           System.Exit
import           System.IO                      (BufferMode (..), hSetBuffering,
                                                 stdout)
import           Control.Monad
import           Data.Proxy
import           GHC.TypeLits
import           Data.Maybe                     (fromJust)

import           Data.Fasta.String.Parse
import           Data.Fasta.String.Types

import           Data.DNA.Assembly
import           Data.DNA.ColoredDeBruijnGraph
import           Data.Enum.Letter

data Args = Single 
  { filePath :: String
  , base :: SomeNat
  }
  | Multiple
  { filePaths :: [String]
  , base :: SomeNat
  }
  | Colored
  { filePaths :: [String]
  , base :: SomeNat
  }

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  args <- parseArgs
  case args of
    (Single fp (SomeNat p@(_ :: Proxy n))) -> runWithArgsSingle p fp
    (Multiple fps (SomeNat p@(_ :: Proxy n))) -> runWithArgsMultiple p fps
    (Colored fps (SomeNat p@(_ :: Proxy n))) -> runWithArgsColored p fps

data AKnownNat (n :: Nat) = forall n. (KnownNat n) => AKnownNat (Proxy n)

runWithArgsSingle :: forall base. KnownNat base => Proxy base -> FilePath -> IO ()
runWithArgsSingle proxy fileName = do
  let baseValue = fromIntegral (natVal proxy)
  putStrLn ("Processing file " <> fileName <> " (using base " <> show baseValue <> ")")
  fastaData <- readFile fileName `as` "Reading file"
  let parsedFasta = parseFasta fastaData
  let readsString = map fastaSeq parsedFasta
  let readsDNASequences = map unsafeLetters readsString
  checkReads baseValue readsDNASequences `as` "Checking reads"
  let deBruijnGraph = graphFromReads readsDNASequences :: DeBruijnGraph base Nucleotide
  print (countUniqueEdges deBruijnGraph)

runWithArgsMultiple :: forall base. KnownNat base => Proxy base -> [FilePath] -> IO ()
runWithArgsMultiple proxy fileNames = do
  let baseValue = fromIntegral (natVal proxy)
  putStrLn ("Processing file [" <> intercalate "," fileNames <> "] (using base " <> show baseValue <> ")")
  fastaDatas <- mapM readFile fileNames `as` "Reading file"
  let parsedFasta = map parseFasta fastaDatas
  let readsString = map (map fastaSeq) parsedFasta
  let readsDNASequences = map (map unsafeLetters) readsString
  _ <- mapM (checkReads baseValue) readsDNASequences `as` "Checking reads"
  let deBruijnGraphs = map graphFromReads readsDNASequences :: [DeBruijnGraph base Nucleotide]
  print (map countUniqueEdges deBruijnGraphs)

runWithArgsColored :: forall base. KnownNat base => Proxy base -> [FilePath] -> IO ()
runWithArgsColored proxy fileNames = do
  let baseValue = fromIntegral (natVal proxy)
  putStrLn ("Processing file [" <> intercalate "," fileNames <> "] (using base " <> show baseValue <> ")")
  fastaDatas <- mapM readFile fileNames `as` "Reading file"
  let parsedFasta = map parseFasta fastaDatas
  let readsString = map (map fastaSeq) parsedFasta
  let readsDNASequences = map (map unsafeLetters) readsString
  _ <- mapM (checkReads baseValue) readsDNASequences `as` "Checking reads"
  let deBruijnGraphs = fromJust (graphsFromReads readsDNASequences) :: ColoredDeBruijnGraph base Nucleotide
  print (countUniqueColoredEdges deBruijnGraphs)


checkReads :: Int -> [ReadSegment] -> IO ()
checkReads k sequences = unless (any ((> k) . length)  sequences) (die "Length of one of the read is less than base")

writeMultiplicityList
  :: (Show b)
  => [b]
  -> IO ()
writeMultiplicityList list = do
  writeFile "data/MultiplicityList.txt" ((unlines . map show) list)
  

as :: IO a -> String -> IO a
command `as` name = do
  putStr (name ++ "...")
  x <- command
  putStrLn "[OK]"
  return x

parseArgs :: IO Args
parseArgs = do
  args <- getArgs
  print args
  case args of 
    ["single", b, file] -> do
                  let intBase = read b
                  return $ Single file (fromJust (someNatVal intBase))
    ("multiple":(b:files)) -> do
                  let intBase = read b
                  return $ Multiple files (fromJust (someNatVal intBase))
    ("uniondiff":(b:files)) -> do
                  let intBase = read b
                  return $ Colored files (fromJust (someNatVal intBase))
    _         -> die "Usage `stack run [single|multiple|uniondiff] [base] [files]`"
                  
