{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge5Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Set1.Challenge5 as s1ch5


main :: IO ()
main = hspec spec


stringToCipher =
  "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

cipher =
  "ICE"

solution =
  "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"


spec :: Spec
spec = do
  describe "Challenge5" $ do
    it "returns a base64 encoded string" $ do
      S1Ch5.challenge5 cipher stringToCipher `shouldBe` solution
