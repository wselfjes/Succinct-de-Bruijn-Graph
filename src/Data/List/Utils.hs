module Data.List.Utils where

import           Data.Function (on)
import           Data.List     (sortBy)

-- | A version of 'nubSort' which operates on a portion of the value.
--
-- > nubSortOn length ["a","test","of","this"] == ["a","of","test"]
--
-- From https://hackage.haskell.org/package/extra
nubSortOn :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn f = nubSortBy (compare `on` f)

-- | A version of 'nubSort' with a custom predicate.
--
-- > nubSortBy (compare `on` length) ["a","test","of","this"] == ["a","of","test"]
--
-- From https://hackage.haskell.org/package/extra
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp = f . sortBy cmp
    where f (x1:x2:xs) | cmp x1 x2 == EQ = f (x1:xs)
          f (x:xs)     = x : f xs
          f []         = []
