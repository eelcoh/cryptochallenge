{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge3Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Set1.Challenges as S1
import qualified Crypto.Attempt as Attempt
import qualified Bytes.Utils as Bytes

main :: IO ()
main = hspec spec


input =
  "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

spec :: Spec
spec = do
  describe "Challenge" $ do
    it "should be crypted with an X" $ do
      (Bytes.w2c $ Attempt.char $ S1.challenge3 input) `shouldBe` 'X'
    it "should read \"Cooking MC's like a pound of bacon\"" $ do
      (Attempt.string $ S1.challenge3 input) `shouldBe` "Cooking MC's like a pound of bacon"
