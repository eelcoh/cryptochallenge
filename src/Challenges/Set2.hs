{-# LANGUAGE OverloadedStrings #-}

module Challenges.Set2
    ( challenge9
    , challenge10
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import qualified Bytes.Utils as Bytes
import qualified Crypto.AES as AES

import Utils.Elmify ((|>))


challenge9 :: Int -> [Char] -> B.ByteString
challenge9 blocksize key =
  Bytes.stringToByteString key
  |> Bytes.pad blocksize       -- see Bytes.Utils

challenge10 :: B.ByteString -> B.ByteString -> B.ByteString
challenge10 key stringToCipher =
  let
    s =
      B64.decodeLenient stringToCipher
  in
    AES.cbcEncrypt key s -- see Crypto.AES
    
