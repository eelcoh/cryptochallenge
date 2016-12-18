module Utils.Xor
  ( fixedXor
  , cycleXor
  , tupleXor
  , xorString
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Word (Word8)

import Utils.Elmify ((|>))
import Utils.Bytes (c2w, w2c, bytesToHex, hexToBytes)
import qualified Data.Bits as BB

xorBytes :: [Char] -> B.ByteString
xorBytes chars =
  map c2w chars
  |> B.pack
  |> B.cycle


fixedXor :: [Char] -> [Char] -> B.ByteString
fixedXor inStr xorStr =
  bytesToHex $ B.pack $ map (uncurry BB.xor) $  B.zip (hexToBytes inStr) (hexToBytes xorStr)


xorByte :: Word8 -> Word8 -> Word8
xorByte =
  BB.xor

tupleXor :: (Word8, Word8) -> Word8
tupleXor =
  uncurry BB.xor

xorString :: B.ByteString -> Char -> [Char]
xorString bStr c =
  let
    bytes =
      xorBytes [c]
  in
    zipWith BB.xor (B.unpack bStr) (B.unpack bytes)
    |> map w2c


cycleXor :: [Char] -> [Char] -> [Char]
cycleXor key chars =
  let
    cycledKey =
      xorBytes key

    bytes =
      map c2w chars

    xorred =
      zipWith BB.xor (B.unpack cycledKey) bytes
      |> B.pack
  in
    bytesToHex xorred
    |> B.unpack
    |> map w2c
