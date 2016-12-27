{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge1Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Challenges.Set1 as S1


main :: IO ()
main = hspec spec


hexString =
  "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

solution =
  "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

spec :: Spec
spec = do
  describe "Challenge1" $ do
    it "returns a base64 encoded string" $ do
      S1.challenge1 hexString `shouldBe` solution
