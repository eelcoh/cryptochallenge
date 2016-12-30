{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge03Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Challenges.Set1 as S1
import qualified Crypto.Xor as Xor
import qualified Utils.Bytes as Bytes

main :: IO ()
main = hspec spec


input =
  "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

spec :: Spec
spec = do
  describe "Challenge" $ do
    it "should be crypted with an X" $ do
      (Bytes.w2c $ Xor.char $ S1.challenge3 input) `shouldBe` 'X'
    it "should read \"Cooking MC's like a pound of bacon\"" $ do
      (Xor.string $ S1.challenge3 input) `shouldBe` "Cooking MC's like a pound of bacon"
