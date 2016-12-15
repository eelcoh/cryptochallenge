{-# LANGUAGE OverloadedStrings #-}

module Utils.Bytes
    ( hexToBytes
    , bytesToHex
    , hexToBase64
    , c2w
    , w2c
    , blocks
    ) where

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as B

import GHC.Base (unsafeChr)
import Data.Char (ord)
import Data.Word (Word8)
import Data.Function

import Utils.Elmify (|>)

hexToBytes :: [Char] -> B.ByteString
hexToBytes str =
  map c2w str
  |> B.Pack
  |> B16.decode
  |> fst


bytesToHex :: B.ByteString -> B.ByteString
bytesToHex =
  B16.encode


hexToBase64 :: [Char] -> B.ByteString
hexToBase64 str =
  hexToBytes str
  |> B64.encode


c2w :: Char -> Word8
c2w = fromIntegral . ord


w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral


blocks :: Int -> B.ByteString -> [B.ByteString]
blocks sz bs =
  B.unpack bs
  |> Split.chunksOf (fromIntegral sz)
  |> map B.pack
