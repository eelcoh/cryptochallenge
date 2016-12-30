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

import Utils.Bytes (hexstringToBase64, stringToByteString, hexStringToByteString, hexStringToByteString, byteStringToHexString)
import Crypto.Xor (fixedXor, cycleKey)

import qualified Crypto.Xor as Xor
import qualified Crypto.AES as AES
import Utils.Strings (substrings, blocks)
import Data.List (sortBy, tails, minimumBy, maximumBy)
import Data.Function (on)
import Data.Ord (Ordering)

import Utils.Elmify ((|>))


challenge1 :: [Char] -> B.ByteString
challenge1 =
  hexstringToBase64           -- see Utils.Bytes


challenge2 :: [Char] -> [Char] -> [Char]
challenge2 s1 s2 =
  let
    -- convert both incoming strings into ByteStrings.
    bs1 =
      hexStringToByteString s1
    bs2 =
      hexStringToByteString s2
  in
    fixedXor bs1 bs2            -- apply Utils.Bytes.fixedXor on them
    |> byteStringToHexString   -- and then convert the result back into a hex string


-- first convert the hexString into a ByteString
-- then apply Crypto.Xor.attempt on it
-- Returns a (best) Match
challenge3 :: [Char] -> Xor.Match
challenge3 =
  Xor.attempt . hexStringToByteString


challenge4 :: [[Char]] -> Xor.Match
challenge4 strings =
  map challenge3 strings
  |> Xor.bestMatch


challenge5 :: [Char] -> [Char] -> [Char]
challenge5 stringToCipher key =
  cycleKey (stringToByteString stringToCipher) (stringToByteString key)
  |> byteStringToHexString


challenge6 :: B.ByteString -> [(B.ByteString, B.ByteString)]
challenge6 contents =
  B64.decodeLenient contents
  |> Xor.search


challenge7 :: B.ByteString -> B.ByteString -> B.ByteString
challenge7 key contents =
  B64.decodeLenient contents
  |> AES.decrypt_key_128 key


challenge8 :: Int -> [[Char]] -> (Int, B.ByteString)
challenge8 key strings =
  map ((AES.detect key) . hexStringToByteString) strings
  |> minimumBy (compare `on` fst)
