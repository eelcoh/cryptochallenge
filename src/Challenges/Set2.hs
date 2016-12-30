{-# LANGUAGE OverloadedStrings #-}

module Challenges.Set2
    ( challenge9
    , challenge10
    , challenge11
    , challenge12
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Map as Map
import Data.Word (Word8)


import qualified Bytes.Utils as Bytes
import qualified Crypto.AES as AES
import System.Random (StdGen)

import Utils.Elmify ((|>))


challenge9 :: Int -> [Char] -> B.ByteString
challenge9 blocksize key =
  Bytes.stringToByteString key
  |> Bytes.pad blocksize       -- see Bytes.Utils

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

challenge12 :: StdGen -> B.ByteString
challenge12 g =
  let
    key =
      AES.make_key g
    buffer =
      "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
      |> B64.decodeLenient

    {- Feed identical bytes of your-string to the function 1 at a time --- start
    with 1 byte ("A"), then "AA", then "AAA" and so on. Discover the block size
    of the cipher. You know it, but do this step anyway. -}
    first_buffers =
      AES.make_buffers key buffer

    -- UNSAFE (but it works in this example)

    key_size =
      map (AES.detect_ecb_key_size 8) first_buffers
      |> filter isJust
      |> map fromJust
      |> minimum


    {- Detect that the function is using ECB. You already know, but do this step
    anyways. -}
    mode =
      B.replicate (key_size * 2) 0x41 -- 'A'
      |> ((flip B.append) buffer)
      |> AES.detect_AES_mode key_size

    {- Knowing the block size, craft an input block that is exactly 1 byte short
    (for instance, if the block size is 8 bytes, make "AAAAAAA"). Think about
    what the oracle function is going to put in that last byte position. -}
    small_block =
      B.replicate (key_size - 1) 0x41

    {- Make a dictionary of every possible last byte by feeding different
    strings to the oracle; for instance, "AAAAAAAA", "AAAAAAAB", "AAAAAAAC",
    remembering the first block of each invocation. -}
    first_block :: B.ByteString -> B.ByteString
    first_block b =
      B.take key_size b

    encrypt :: B.ByteString -> B.ByteString
    encrypt b =
      AES.ecb_encrypt key b

    entry :: B.ByteString -> (B.ByteString, Word8)
    entry b =
      (first_block . encrypt) b
      |> (flip (,)) (B.last b)

    dir :: Map.Map B.ByteString Word8
    dir =
      map (B.snoc small_block) Bytes.all_chars
      |> map entry
      |> Map.fromList


    {- Match the output of the one-byte-short input to one of the entries in
    your dictionary. You've now discovered the first byte of unknown-string. -}
    {- Repeat for the next byte. -}

    solution =
      B.tails buffer
      |> init
      |> map (B.append small_block)
      |> map encrypt
      |> map first_block
      |> map ((flip Map.lookup) dir)
      |> catMaybes
      |> B.pack

  in
    solution
