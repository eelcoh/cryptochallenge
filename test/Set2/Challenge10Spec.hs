{-# LANGUAGE OverloadedStrings #-}
module Set2.Challenge10Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Challenges.Set2 as S2

import Crypto.AES

main :: IO ()
main = hspec spec


key =
  "YELLOW SUBMARINE"

solution =
  "YELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINE"

spec :: Spec
spec = do
  describe "Challenge 10" $ do
    it "(given a string is n times keysize long) should encrypt and decrypt a string back and forth" $ do
      (cbcDecrypt key $ cbcEncrypt key solution ) `shouldBe` solution
