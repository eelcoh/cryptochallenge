{-# LANGUAGE OverloadedStrings #-}

module Crypto.AES
    ( init_AES_128
    , crypt_key_128
    , decrypt_key_128
    , detect_ecb_key_size
    , detect
    , detect_AES_mode
    , cbc_encrypt
    , cbc_decrypt
    , ecb_encrypt
    , encryption_oracle
    , encryption_oracles
    , Mode
    , make_key
    , make_buffers
    ) where

import qualified Data.ByteString as B

-- import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Cipher as C

import System.Random

import Utils.Bytes as Bytes
import Bytes.Xor as Xor
import Utils.List (pairs)
import Data.List (last)
import Data.Word (Word8)

import Stats.HammingDistance as Hamming

import Utils.Elmify ((|>))

data Mode
  = EBC
  | CBC
  deriving (Show, Enum, Bounded)

instance Random Mode where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- the bytestring need to have a length of 16 bytes
-- otherwise the simplified error handling will raise an exception.
init_AES_128 :: B.ByteString -> C.AES128
init_AES_128 = either (error . show) C.cipherInit . C.makeKey

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
initAES256 :: B.ByteString -> C.AES256
initAES256 = either (error . show) C.cipherInit . C.makeKey


-- ECB
-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
cryptKey256 key msg = C.ecbEncrypt ctx msg
  where ctx = initAES256 key

decrypt_key_128 key msg = C.ecbDecrypt ctx msg
  where ctx = init_AES_128 key

crypt_key_128 key msg = C.ecbEncrypt ctx msg
  where ctx = init_AES_128 key

get_randoms :: StdGen -> Int -> (B.ByteString, StdGen)
get_randoms g keysz =
  let
    create_randoms =
      iterate (\rg -> random (snd rg) :: (Word8, StdGen))

    to_byte_string rgs =
      map fst rgs
      |> B.pack
      |> (flip (,)) (last rgs |> snd)

  in
    create_randoms (0x00, g)
    |> take keysz
    |> to_byte_string


encryption_oracles :: StdGen -> Int -> B.ByteString -> [B.ByteString]
encryption_oracles g num string_to_cipher =
  let
    oracle f =
      iterate (\rg -> (f (snd rg))) (string_to_cipher, g)
    encr =
      (flip encryption_oracle) string_to_cipher
  in
    oracle encr
    |> take num
    |> map fst
    |> tail


encryption_oracle :: StdGen -> B.ByteString -> (B.ByteString, StdGen)
encryption_oracle g0 str =
  let
    keysz =
      16

    (key, g1) =
      get_randoms g0 keysz

    (numBefore, g2) =
      randomR (5, 10) g1

    (numAfter, g3) =
      randomR (5, 10) g2

    (pad_before, g4) =
      get_randoms g3 numBefore

    (pad_after, g5) =
      get_randoms g4 numAfter

    (iv, g6) =
      get_randoms g5 keysz

    (mode, g7) =
      random g6 :: (Mode, StdGen)

    string_to_cipher =
      B.concat [pad_before, str, pad_after]

  in
    case mode of
      CBC ->
        (cbc_encrypt key iv string_to_cipher, g7)
      EBC ->
        (ecb_encrypt key string_to_cipher, g7)

{-
decrypt_buffer :: B.ByteString -> B.ByteString -> (B.ByteString, StdGen)
decrypt_buffer key buffer =
  let
    string_to_cipher =
      B.append buffer pad_after

  in
    ecb_encrypt key string_to_cipher
-}

make_key :: StdGen -> B.ByteString
make_key g =
  let
    keysz =
      16
    (key, _) =
      get_randoms g keysz
  in
    key


make_buffers :: B.ByteString -> B.ByteString -> [B.ByteString]
make_buffers key buffer =
  let
    max_size =
      64
    max_string =
      B.replicate (2 * max_size + 1) 0x41 -- 0x41 is 'A'
  in
    B.inits max_string
    |> map (\b -> B.append b buffer)
    |> map (ecb_encrypt key)

{-
find_key_size :: [B.ByteString] -> Int
find_key_size key buffer =

    |> map (\(b1, b2) -> ((B.length b1), (detect (B.length b1) b2))
    |> filter (\(_, (dst, _)) -> dst == 0)
    |> maximumBy (compare `on` fst)
    |> fst

-}






{-
encrypt_buffer :: B.ByteString -> B.ByteString -> (B.ByteString, StdGen)
encrypt_buffer key buffer =
  let
    pad_after =
      "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
      |> B64.decode

    string_to_cipher =
      B.append buffer pad_after

  in
    ecb_encrypt key string_to_cipher
-}

detect_AES_mode :: Int -> B.ByteString -> Mode
detect_AES_mode keysz str =
  let
    (dst, _) =
      detect keysz str
  in
    if dst == 0
      then
        EBC
      else
        CBC

detect_ecb_key_size :: Int -> B.ByteString -> Maybe Int
detect_ecb_key_size keysz buffer =
  let
    is_found (i, bs) =
      i == 0

    max_key_size =
      128
  in
    if keysz > max_key_size
      then
        Nothing
      else
        case (detect_ecb_key_size (keysz * 2) buffer) of

          Just s ->
            Just s

          Nothing ->
            if (detect keysz buffer |> is_found)     -- UNSAFE (but it works in this example with max_key_size of 128)
              then
                Just keysz
              else
                Nothing



detect :: Int -> B.ByteString -> (Int, B.ByteString)
detect key str =
  Bytes.blocks key str
  |> pairs
  |> map Hamming.bytestrings
  |> minimum
  |> ((flip (,)) str)


ecb_encrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecb_encrypt key string_to_cipher =
  let
    ctx =
      init_AES_128 key

    keysz =
      B.length key

  in
    Bytes.blocks keysz string_to_cipher
    |> map ((C.ecbDecrypt ctx) . (Bytes.pad keysz))
    |> B.concat

-- https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#CBC
cbc_encrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbc_encrypt key iv string_to_cipher =
  let
    keysz =
      B.length key

    ctx =
      init_AES_128 key

    blocks_to_cipher =
      Bytes.blocks keysz string_to_cipher
      |> map (Bytes.pad keysz)

  in
    cbc ctx iv blocks_to_cipher
    |> B.concat


cbc :: C.AES128 -> B.ByteString -> [B.ByteString] -> [B.ByteString]
cbc ctx iv blocks_to_cipher =
  case blocks_to_cipher of
    [] ->
      []
    (x:xs) ->
      let
        first_block =
          Xor.fixedXor iv x
          |> C.ecbEncrypt ctx

        next_blocks =
          cbc ctx first_block xs

      in
        first_block:next_blocks

cbc_decrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbc_decrypt key iv string_to_decipher =
  let
    keysz =
      B.length key

    ctx =
      init_AES_128 key

    blocks_to_decipher =
      Bytes.blocks keysz string_to_decipher

    blocks_decipherd =
      map (C.ecbDecrypt ctx) blocks_to_decipher

    blocks_to_xor =
      (iv, (head blocks_decipherd)) : (zip blocks_to_decipher (tail blocks_decipherd))

  in
    map (uncurry Xor.fixedXor) blocks_to_xor
    |> B.concat
