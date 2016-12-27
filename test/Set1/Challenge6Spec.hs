{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge6Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Data.ByteString as B
import qualified Challenges.Set1 as S1


main :: IO ()
main = hspec spec

str2decrypt =
  "CzY3JyorLmNiLC5paSojaToqPGMkIC1iPWM0PComImMkJydlJyooKy8gQwplLixlKjEkMzplPisgJ2MMaSsgKDFlKGMmMC4nKC8="

key =
  "Terminator X: Bring the noise"
solution =
  "I'm back and I'm ringin' the bell \nA rockin' on the mike while t"


spec :: Spec
spec = do
  describe "Challenge6" $ do
    fileContents <- runIO (B.readFile "./static/6.txt")
    it "finds the key" $ do
      (elem key $ map fst $ S1.challenge6 fileContents) `shouldBe` True
