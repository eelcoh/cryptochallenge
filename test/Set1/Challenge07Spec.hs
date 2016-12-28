{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge07Spec
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
  "YELLOW SUBMARINE"
solution =
  "I'm back and I'm ringin' the bell \nA rockin' on the mike while t"


spec :: Spec
spec = do
  describe "Challenge 7" $ do
    fileContents <- runIO (B.readFile "./static/7.txt")
    it "decrypts an AES ECB encrpted text" $ do
      (B.take 64 $ S1.challenge7 key fileContents) `shouldBe` solution
