{-# LANGUAGE OverloadedStrings #-}

module Crypto.AES
    ( initAES128
    , cryptKey128
    , decryptKey128
    , detect
    ) where

import qualified Data.ByteString as B

--import qualified Crypto.Cipher as C
import Crypto.Cipher.Types
import Crypto.Cipher

import Bytes.Utils as Bytes
import Utils.List (pairs)

import Stats.HammingDistance as Hamming

import Utils.Elmify ((|>))

-- the bytestring need to have a length of 16 bytes
-- otherwise the simplified error handling will raise an exception.
initAES128 :: B.ByteString -> AES128
initAES128 = either (error . show) cipherInit . makeKey

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
initAES256 :: B.ByteString -> AES256
initAES256 = either (error . show) cipherInit . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
cryptKey256 key msg = ecbEncrypt ctx msg
  where ctx = initAES256 key

decryptKey128 key msg = ecbDecrypt ctx msg
  where ctx = initAES128 key

cryptKey128 key msg = ecbEncrypt ctx msg
  where ctx = initAES128 key

detect :: Int -> [Char] -> (Int, [Char])
detect key hexString =
  Bytes.hexStringToByteString hexString
  |> Bytes.blocks key
  |> pairs
  |> map Hamming.bytestrings
  |> minimum
  |> ((flip (,)) hexString)
