{-# LANGUAGE OverloadedStrings #-}
module Set2.Challenge9Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Challenges.Set2 as S2


main :: IO ()
main = hspec spec


key =
  "YELLOW SUBMARINE"

solution =
  "YELLOW SUBMARINE\x04\x04\x04\x04"

spec :: Spec
spec = do
  describe "Challenge 9" $ do
    it "pads the key" $ do
      S2.challenge9 20 key `shouldBe` solution
