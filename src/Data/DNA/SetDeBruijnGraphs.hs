{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.DNA.SetDeBruijnGraphs where

import           GHC.TypeLits

import           Data.DNA.Assembly
import           Data.RankSelect.Maps as RSMaps


-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Data.Enum.Letter

newtype ColoredDeBruijnGraph n a = GenomesSet {getMaps :: RSMaps.RankSelectMaps (Edge n a) Int}

instance (Show (Edge n a), Bounded a, Enum a, KnownNat (n + 1))
  => Show (ColoredDeBruijnGraph n a) where
  show = show . toMultiplicityLists

-- | Convert set of de Bruijn Graphs into list of edges and counts
toMultiplicityLists
  :: (Bounded a, Enum a, KnownNat (n + 1))
  => ColoredDeBruijnGraph n a
  -> [[(Edge n a, Int)]]
toMultiplicityLists = RSMaps.toListsBoundedEnum . getMaps


graphsFromReads
  :: [[ReadSegment]]
  -> ColoredDeBruijnGraph n a
graphsFromReads reads = GenomesSet RSMaps.empty

-- | Union of two de Bruijn graphs
--
-- >>> unionOfTwoGraphs (graphFromReads @2 [unsafeLetters @"ACGT" "AAACCAACC"]) (graphFromReads @2 [unsafeLetters @"ACGT" "AAACGAACC"])
-- [[("AAA",1),("AAC",2),("ACC",2),("CAA",1),("CCA",1)],[("AAA",1),("AAC",2),("ACC",1),("ACG",1),("CGA",1),("GAA",1)]]
-- >>> (commonPart . getMaps) (unionOfTwoGraphs (graphFromReads @2 [unsafeLetters @"ACGT" "AAACCAACC"]) (graphFromReads @2 [unsafeLetters @"ACGT" "AAACGAACC"]))
-- 11000100000000000000000000000000000000000000000000000000000000000
unionOfTwoGraphs
  :: (KnownNat n, KnownNat (n+1), Bounded a, Enum a)
  => DeBruijnGraph n a
  -> DeBruijnGraph n a
  -> ColoredDeBruijnGraph n a
unionOfTwoGraphs (DeBruijnGraph first) (DeBruijnGraph second) = GenomesSet (RSMaps.unionOfTwoMaps first second)


addReadsToGraph
  :: [ReadSegment]
  -> ColoredDeBruijnGraph n a
  -> ColoredDeBruijnGraph n a
addReadsToGraph _ graph = graph
