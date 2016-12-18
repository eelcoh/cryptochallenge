{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge4Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Set1.Challenges as S1
import qualified Crypto.Attempt as Attempt


main :: IO ()
main = hspec spec


result =
  "Now that the party is jumping\n"

spec :: Spec
spec = do
  describe "Challenge 4" $ do
    it ("should find the one string that when decrypted says " ++ result) $ do
      (Attempt.string S1.challenge4) `shouldBe` result
