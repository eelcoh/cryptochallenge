{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge3Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Set1.Challenge3 as C3


main :: IO ()
main = hspec spec


input =
  "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

spec :: Spec
spec = do
  describe "Challenge" $ do
    it "should be crypted with an X" $ do
      (C3.char $ C3.challenge input) `shouldBe` 'X'
    it "should read \"Cooking MC's like a pound of bacon\"" $ do
      (C3.string $ C3.challenge input) `shouldBe` "Cooking MC's like a pound of bacon"
