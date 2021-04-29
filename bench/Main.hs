{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Monad
import Control.Monad.ST
import Data.Proxy
import Data.Int
import Criterion.Main
import Data.RankSelect.Map
import Data.Enum.Utils

rankSelectMapSmall = fromListAscN fromBoundedEnum 256 256 randomList
    where
        randomList = zip ([-128..127]::[Int8]) (cycle ['a'..'z'])
rankSelectMapBig = fromListAscN id 16135325324 100000 randomList 
    where
        randomList = zip ([0..100000]::[Int]) (cycle ['A'..'z'])

rankTest :: (Bounded k, Enum k) => k -> RankSelectMap k v -> Int
rankTest k m = runST $ do 
    m' <- return m
    return $ rank k m'

main :: IO ()
main = do
    defaultMain [
        bgroup "rankTest"
            [ bench "256 Low key"                       $ whnf (rankTest 2)     rankSelectMapSmall
            , bench "256 High key"                      $ whnf (rankTest 20)    rankSelectMapSmall
            , bench "100k Low key"                      $ whnf (rankTest 100)   rankSelectMapBig
            , bench "100k High key"                     $ whnf (rankTest 70000) rankSelectMapBig
        ]
        ]