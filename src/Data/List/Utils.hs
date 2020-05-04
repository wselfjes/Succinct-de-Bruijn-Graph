module Data.List.Utils where

import           Data.Function (on)
import           Data.List     (sortBy, tails)

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

nubSortOnWith :: Ord b => (a -> a -> a) -> (a -> b) -> [a] -> [a]
nubSortOnWith combine f = nubSortByWith combine (compare `on` f)

nubSortByWith :: (a -> a -> a) -> (a -> a -> Ordering) -> [a] -> [a]
nubSortByWith combine cmp = f . sortBy cmp
    where f (x1:x2:xs) | cmp x1 x2 == EQ = f (combine x1 x2 : xs)
          f (x:xs)     = x : f xs
          f []         = []

-- | Lazily and efficiently split sequence
-- into overlapping chunks of given size.
--
-- >>> chunksOf 3 "hello"
-- ["hel","ell","llo"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = map (take n) (dropEnd n (tails xs))

-- | Lazily drop last @n@ elements from a list.
--
-- >>> dropEnd 2 [1..5]
-- [1,2,3]
--
-- This is morally equialent to @reverse . drop n . reverse@,
-- but instead relies on a "sliding window" of size @n@ to
-- guarantee efficient and lazy traversal of input.
--
-- >>> take 10 (dropEnd 100 [1..])
-- [1,2,3,4,5,6,7,8,9,10]
dropEnd :: Int -> [a] -> [a]
dropEnd n = go []
  where
    go [] ys         = go xs' ys'
      where (xs', ys') = splitAt n ys
    go _ []          = []
    go (x:xs) (y:ys) = x : go (xs ++ [y]) ys

