module Crypto.Attack
  ( attack
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Word (Word8)

import qualified Crypto.AES as AES

import qualified Utils.Bytes as Bytes
import Utils.Elmify ((|>))


attack :: (B.ByteString -> B.ByteString) -> B.ByteString -> Maybe B.ByteString
attack encrypt_fn buffer =
  let
    {- find key size, returns a Maybe -}
    m_key_size =
      AES.detect_ecb_key_size encrypt_fn buffer


    {- Detect that the function is using ECB. You already know, but do this step
    anyways. -}
    m_mode =
      fmap (detect_AES_mode buffer) m_key_size

  in
    fmap (attack' encrypt_fn buffer) m_key_size

attack' :: (B.ByteString -> B.ByteString) -> B.ByteString -> Int -> B.ByteString
attack' encrypt_fn buffer key_size =
  let
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

    entry :: B.ByteString -> (B.ByteString, Word8)
    entry b =
      (first_block . encrypt_fn) b
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
      |> map encrypt_fn
      |> map first_block
      |> map ((flip Map.lookup) dir)
      |> catMaybes
      |> B.pack
  in
    solution

detect_AES_mode :: B.ByteString -> Int -> AES.Mode
detect_AES_mode buffer key_size =
  B.replicate (key_size * 2) 0x41 -- 'A'
  |> ((flip B.append) buffer)
  |> AES.detect_AES_mode key_size
