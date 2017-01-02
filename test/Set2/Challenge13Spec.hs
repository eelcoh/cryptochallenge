{-# LANGUAGE OverloadedStrings #-}
module Set2.Challenge13Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.Random (getStdGen)

import Challenges.Set2 as S2
import Utils.Profiles (parse_kvs_string, parse_kv_string, profile, encode)

import qualified Data.ByteString as B
import Crypto.AES

import qualified Data.Map as Map
import Data.List (sortBy)

main :: IO ()
main = hspec spec


key =
  "YELLOW SUBMARINE"

solution =
  "YELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINEYELLOW SUBMARINE"

kvs =
  "foo=bar&baz=qux&zap=zazzle"
kvs' =
  "foo=bar&&baz=qux&zap=zazzle"


spec :: Spec
spec = do
  describe "Challenge 13" $ do
    generator <- runIO (getStdGen)
    it "given a key value string parses it into keys and values" $ do
      (parse_kv_string "key=value" ) `shouldBe` Just ("key", "value")
    it "given a whitespaced key value string parses it into key and value" $ do
      (parse_kv_string " key=value " ) `shouldBe` Just ("key", "value")
    it "given an invalid key value string will not parse it" $ do
      (parse_kv_string "key==value" ) `shouldBe` Nothing
    it "given an key values string parses it into a list of key and value tuples" $ do
      Map.lookup "baz" (parse_kvs_string kvs) `shouldBe` Just "qux"
    it "given an key values string parses it into a list of key and value tuples" $ do
      Map.lookup "foo" (parse_kvs_string kvs') `shouldBe` Just "bar"
    it "given an key values string parses it into a list of key and value tuples" $ do
      Map.lookup "baz" (parse_kvs_string kvs') `shouldBe` Just "qux"
    it "given an key values string parses it into a list of key and value tuples" $ do
      Map.lookup "zap" (parse_kvs_string kvs') `shouldBe` Just "zazzle"
    it "ignores '=' and '&' in the input for a profile" $ do
      Map.lookup "email" (profile "foo=@bar&.com") `shouldBe` Just "foo@bar.com"
    it "encodes a profile" $ do
      encode (profile "foo@bar.com") `shouldBe` "email=foo@bar.com&uid=10&role=user"
    it "can be hacked with an email address of length 13" $ do
      Map.lookup "role" (S2.challenge13 generator "fool1@bar.com") `shouldBe` Just "admin"
