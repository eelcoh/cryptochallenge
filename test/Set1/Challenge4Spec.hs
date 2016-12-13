{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge4Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Set1.Challenge4 as C4


main :: IO ()
main = hspec spec


result =
  "Now that the party is jumping\n"

spec :: Spec
spec = do
  describe "Challenge 4" $ do
    it ("should find the one string that when decrypted says " ++ result) $ do
      (C4.string C4.challenge) `shouldBe` result
