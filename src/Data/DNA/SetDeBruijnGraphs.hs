{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.DNA.SetDeBruijnGraphs where

import           GHC.TypeLits

import           Data.DNA.Assembly
import           Data.RankSelect.Maps as RSMaps



newtype ColoredDeBruijnGraph n a = GenomesSet {getMaps :: RSMaps.RankSelectMaps (Edge n a) Int}

instance (Show (Edge n a), Bounded a, Enum a, KnownNat (n + 1))
  => Show (ColoredDeBruijnGraph n a) where
  show = show . toMultiplicityLists

toMultiplicityLists
  :: (Bounded a, Enum a, KnownNat (n + 1))
  => ColoredDeBruijnGraph n a
  -> [[(Edge n a, Int)]]
toMultiplicityLists = RSMaps.toListsBoundedEnum . getMaps


graphFromReads
  :: [[ReadSegment]]
  -> ColoredDeBruijnGraph n a
graphFromReads _ = GenomesSet RSMaps.empty


addReadsToGraph
  :: [ReadSegment]
  -> ColoredDeBruijnGraph n a
  -> ColoredDeBruijnGraph n a
addReadsToGraph _ graph = graph
