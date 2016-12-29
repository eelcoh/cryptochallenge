{-# LANGUAGE OverloadedStrings #-}

module Challenges.Set2
    ( challenge9
    , challenge10
    , challenge11
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import qualified Bytes.Utils as Bytes
import qualified Crypto.AES as AES
import System.Random (StdGen)

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

    iv =
      B.replicate (B.length key) 0x00
  in
    AES.cbcEncrypt key iv s -- see Crypto.AES

challenge11 :: StdGen -> B.ByteString -> [(AES.Mode, B.ByteString)]
challenge11 g stringToCipher =
  AES.encryption_oracles g 20 stringToCipher
  |> map (\b -> ((AES.detect_oracle 16 b), b))
