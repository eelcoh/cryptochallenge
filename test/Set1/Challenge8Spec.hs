{-# LANGUAGE OverloadedStrings #-}
module Set1.Challenge5Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Set1.Challenges as S1


main :: IO ()
main = hspec spec

solution =
  "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a"


spec :: Spec
spec = do
  describe "Challenge5" $ do
    fileContents <- readFile "./static/8.txt"
    it "detects the string that was encrypted with AES ECB mode" $ do
      (snd $ S1.challenge8 16 fileContents) `shouldBe` solution
