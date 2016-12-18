{-# LANGUAGE OverloadedStrings #-}

module Bytes.Utils
    ( stringToByteString
    , byteStringToString
    , hexStringToByteString
    , byteStringToHexString
    , hexstringToBase64
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
import qualified Data.List.Split as Split

import Utils.Elmify ((|>))

-- CONVERSION
stringToByteString :: [Char] -> B.ByteString
stringToByteString str =
  map c2w str
  |> B.pack

byteStringToString :: B.ByteString -> [Char]
byteStringToString bs =
  B.unpack bs
  |> map w2c

c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral

-- convert strings like "37f384" into a bytestring with that hexadecimal value
hexStringToByteString :: [Char] -> B.ByteString
hexStringToByteString str =
  map c2w str
  |> B.pack
  |> B16.decode
  |> fst

  -- convert a bytestring with a hexadecimal value into a string
byteStringToHexString :: B.ByteString -> [Char]
byteStringToHexString bStr =
  B16.encode bStr
  |> B.unpack
  |> map w2c


hexstringToBase64 :: [Char] -> B.ByteString
hexstringToBase64 str =
  hexStringToByteString str
  |> B64.encode

-- chop into bytestrings of sz bytes
blocks :: Int -> B.ByteString -> [B.ByteString]
blocks sz bs =
  B.unpack bs
  |> Split.chunksOf (fromIntegral sz)
  |> map B.pack