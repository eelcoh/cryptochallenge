{-# LANGUAGE OverloadedStrings #-}
module Set2.Challenge10Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Challenges.Set2 as S2

import qualified Data.ByteString as B
import Crypto.AES

main :: IO ()
main = hspec spec


key =
  "YELLOW SUBMARINE"

solution =
  "YELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINE"

iv =
  B.replicate (B.length key) 0x00

spec :: Spec
spec = do
  describe "Challenge 10" $ do
    it "(given a string is n times keysize long) should encrypt and decrypt a string back and forth" $ do
      (cbcDecrypt key iv $ cbcEncrypt key iv solution ) `shouldBe` solution
