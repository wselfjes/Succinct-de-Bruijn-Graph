
module Data.DNA.SetDeBruijnGraphs where

import Data.DNA.Assembly
import Data.RankSelect.Maps as RSMaps



newtype MultipleDeBruijnGraph = GenomesSet {getMaps :: RSMaps.RankSelectMaps Nucleotide Int}


