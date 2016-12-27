{-# LANGUAGE OverloadedStrings #-}

module Crypto.AES
    ( initAES128
    , cryptKey128
    , decryptKey128
    , detect
    , cbcEncrypt
    , cbcDecrypt
    ) where

import qualified Data.ByteString as B

-- import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Cipher as C

import Bytes.Utils as Bytes
import Bytes.Xor as Xor
import Utils.List (pairs)

import Stats.HammingDistance as Hamming

import Utils.Elmify ((|>))

-- the bytestring need to have a length of 16 bytes
-- otherwise the simplified error handling will raise an exception.
initAES128 :: B.ByteString -> C.AES128
initAES128 = either (error . show) C.cipherInit . C.makeKey

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
initAES256 :: B.ByteString -> C.AES256
initAES256 = either (error . show) C.cipherInit . C.makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
cryptKey256 key msg = C.ecbEncrypt ctx msg
  where ctx = initAES256 key

decryptKey128 key msg = C.ecbDecrypt ctx msg
  where ctx = initAES128 key

cryptKey128 key msg = C.ecbEncrypt ctx msg
  where ctx = initAES128 key

detect :: Int -> [Char] -> (Int, [Char])
detect key hexString =
  Bytes.hexStringToByteString hexString
  |> Bytes.blocks key
  |> pairs
  |> map Hamming.bytestrings
  |> minimum
  |> ((flip (,)) hexString)


cbcEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
cbcEncrypt key stringToCipher =
  let
    keysz =
      B.length key

    iv =
      B.replicate keysz 0x00

    ctx =
      initAES128 key

    blocksToCipher =
      Bytes.blocks keysz stringToCipher
      |> map (Bytes.pad keysz)

    lengthOK x =
      B.length x == 16

  in
    cbc ctx iv blocksToCipher
    |> B.concat


cbc :: C.AES128 -> B.ByteString -> [B.ByteString] -> [B.ByteString]
cbc ctx iv blocksToCipher =
  case blocksToCipher of
    [] ->
      []
    (x:xs) ->
      let
        firstBlock =
          Xor.fixedXor iv x
          |> C.ecbEncrypt ctx

        nextBlocks =
          cbc ctx firstBlock xs

      in
        firstBlock:nextBlocks

cbcDecrypt :: B.ByteString -> B.ByteString -> B.ByteString
cbcDecrypt key stringToDecipher =
  let
    keysz =
      B.length key

    ctx =
      initAES128 key

    blocksToDecipher =
      Bytes.blocks keysz stringToDecipher

    blocksDecipherd =
      map (C.ecbDecrypt ctx) blocksToDecipher

    iv =
      B.replicate keysz 0x00

    blocksToXor =
      (iv, (head blocksDecipherd)) : (zip blocksToDecipher (tail blocksDecipherd))

  in
    map (uncurry Xor.fixedXor) blocksToXor
    |> B.concat
