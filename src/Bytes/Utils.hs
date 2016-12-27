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
    , pad
    ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B

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
  map c2w str     -- [Char] -> [Word8]
  |> B.pack       -- [Word8] -> ByteString
  |> B16.decode   -- from Hex (Base16) to (ByteString, ByteString) (snd is for characters failing the parsing)
  |> fst          -- take the first from the tuple

  -- convert a bytestring with a hexadecimal value into a string
byteStringToHexString :: B.ByteString -> [Char]
byteStringToHexString bStr =
  B16.encode bStr -- binary -> Base16
  |> B.unpack     -- ByteString -> [Word8]
  |> map w2c      -- [Word8] -> [Char]


hexstringToBase64 :: [Char] -> B.ByteString
hexstringToBase64 str =
  hexStringToByteString str -- convert to ByteString first
  |> B64.encode             -- encode Base64

-- utility to chop a ByteString into a list of bytestrings of sz bytes
blocks :: Int -> B.ByteString -> [B.ByteString]
blocks sz bs =
  B.unpack bs
  |> Split.chunksOf (fromIntegral sz)
  |> map B.pack

pad :: Int -> B.ByteString -> B.ByteString
pad blksz key =
  let
    padByte =
      0x04::Word8

    keysz =
      B.length key

    padding =
      B.replicate blksz padByte
      |> B.drop keysz
  in
    B.append key padding
