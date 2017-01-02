{-# LANGUAGE OverloadedStrings #-}

module Challenges.Set2
    ( challenge9
    , challenge10
    , challenge11
    , challenge12
    , challenge13
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Map as Map
import Data.Word (Word8)


import qualified Utils.Bytes as Bytes
import qualified Crypto.AES as AES
import qualified Crypto.Attack as Attack
import System.Random (StdGen)

import Utils.Elmify ((|>))

import qualified Utils.Profiles as Profiles


challenge9 :: Int -> [Char] -> B.ByteString
challenge9 blocksize key =
  Bytes.stringToByteString key
  |> Bytes.pad blocksize       -- see Utils.Bytes

challenge10 :: B.ByteString -> B.ByteString -> B.ByteString
challenge10 key string_to_cipher =
  let
    s =
      B64.decodeLenient string_to_cipher

    iv =
      B.replicate (B.length key) 0x00
  in
    AES.cbc_encrypt key iv s -- see Crypto.AES

challenge11 :: StdGen -> B.ByteString -> [(AES.Mode, B.ByteString)]
challenge11 g string_to_cipher =
  AES.encryption_oracles g 20 string_to_cipher
  |> map (\b -> ((AES.detect_AES_mode 16 b), b))

challenge12 :: StdGen -> Maybe B.ByteString
challenge12 g =
  let
    key =
      AES.make_key g

    buffer =
      "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
      |> B64.decodeLenient

    encrypt_fn =
      AES.ecb_encrypt key
  in
    Attack.attack encrypt_fn buffer


challenge13 :: StdGen -> String -> Map.Map String String
challenge13 g profile_name =
  let
    key =
      AES.make_key g

    -- partial function to 'store' the key in
    -- profile_for :: String -> B.ByteString
    --profile =
    --  Profiles.profile_for profile_name

    encrypt_fn =
      Profiles.profile_for' key

    decrypt_fn =
      AES.ecb_decrypt key

    --enc_profile =
    --  encrypt_fn profile

    dec_profile =
      Profiles.attack encrypt_fn profile_name
      |> decrypt_fn

  in
    dec_profile
    |> Bytes.byteStringToString
    |> Profiles.parse_kvs_string
