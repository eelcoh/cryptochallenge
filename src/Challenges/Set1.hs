{-# LANGUAGE OverloadedStrings #-}

module Challenges.Set1
    ( challenge1
    , challenge2
    , challenge3
    , challenge4
    , challenge5
    , challenge6
    , challenge7
    , challenge8
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import Bytes.Utils (hexstringToBase64, stringToByteString, hexStringToByteString, byteStringToHexString)
import Bytes.Xor (fixedXor, cycleKey)

import qualified Crypto.Attempt as Attempt
import qualified Crypto.Key as Key
import qualified Crypto.AES as AES
import Utils.Strings (substrings, blocks)
import Data.List (sortBy, tails, minimumBy, maximumBy)
import Data.Function (on)
import Data.Ord (Ordering)

import Utils.Elmify ((|>))


challenge1 :: [Char] -> B.ByteString
challenge1 =
  hexstringToBase64           -- see Bytes.Utils


challenge2 :: [Char] -> [Char] -> [Char]
challenge2 s1 s2 =
  let
    -- convert both incoming strings into ByteStrings.
    bs1 =
      hexStringToByteString s1
    bs2 =
      hexStringToByteString s2
  in
    fixedXor bs1 bs2            -- apply Bytes.Utils.fixedXor on them
    |> byteStringToHexString   -- and then convert the result back into a hex string


-- first convert the hexString into a ByteString
-- then apply Crypto.Attempt.attempt on it
-- Returns a (best) Match
challenge3 :: [Char] -> Attempt.Match
challenge3 =
  Attempt.attempt . hexStringToByteString


challenge4 :: [[Char]] -> Attempt.Match
challenge4 strings =
  map challenge3 strings
  |> Attempt.bestMatch


challenge5 :: [Char] -> [Char] -> [Char]
challenge5 stringToCipher key =
  cycleKey (stringToByteString stringToCipher) (stringToByteString key)
  |> byteStringToHexString


challenge6 :: B.ByteString -> [(B.ByteString, B.ByteString)]
challenge6 contents =
  B64.decodeLenient contents
  |> Key.search


challenge7 :: B.ByteString -> B.ByteString -> B.ByteString
challenge7 key contents =
  B64.decodeLenient contents
  |> AES.decryptKey128 key


challenge8 :: Int -> [[Char]] -> (Int, [Char])
challenge8 key strings =
  map (AES.detect key) strings
  |> minimumBy (compare `on` fst)
