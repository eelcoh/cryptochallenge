{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Set1.Challenge1 as S1Ch1


hexString =
  "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

solution =
  "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

main :: IO ()
main = hspec $ do
  describe "Challenge-1" $ do
    it "returns a base64 encoded string" $ do
      S1Ch1.hexToBase64 hexString `shouldBe` solution

--main :: IO ()
--main = putStrLn "Test suite not yet implemented"
-}
