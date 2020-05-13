{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.DNA.AssemblySpec where

import           Test.Hspec
import           Data.Proxy
import           Data.DNA.Assembly
import           Data.Enum.Utils
import           Data.Enum.Letter


--testSuccessor :: IO ()
--testSuccessor = successors `shouldBe` []
--  where
--    deBruijnGraph = graphFromReads @2 [unsafeLetters @"ACTG" "AAACCAACC"]
--    successors = successorEdges deBruijnGraph ("AC" :: Node 1 "ACGT" )

--testNodeVal :: IO ()
--testNodeVal = base `shouldBe` 4 
--  where
--    --nodeVal = base * fromEnum ("AC":: Node @2 @"ACGT")
--    base = boundedEnumSize (Proxy @"ACGT")


spec :: Spec
spec =
  describe "Tests for Assembly" $ do
    it "Test nodeVal" testNodeVal
