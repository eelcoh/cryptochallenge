{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge2Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Set1.Challenges as S1


main :: IO ()
main = hspec spec


input =
  "1c0111001f010100061a024b53535009181c"

xor =
  "686974207468652062756c6c277320657965"
  -- "hit the bull's eye"

solution =
  "746865206b696420646f6e277420706c6179"
  -- "the kid don't play"


spec :: Spec
spec = do
  describe "Challenge1" $ do
    it "returns a base64 encoded string" $ do
      S1.challenge2 input xor `shouldBe` solution
