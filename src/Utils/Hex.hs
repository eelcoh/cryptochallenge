{-# LANGUAGE OverloadedStrings #-}

module Utils.Hex
    ( hexToBytes
    , bytesToHex
    , hexToBase64
    , fixedXor
    , c2w
    , w2c
    , xorByte
    , xorString
    , cycleCrypt
    ) where

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.Base64 as B64
-- import qualified Data.ByteString.Base16 as B16
-- import qualified Data.ByteString as B
import qualified Data.Bits as BB
import GHC.Base (unsafeChr)
import Data.Char (ord)
import Data.Word (Word8)
import Utils.Elmify


hexToBytes :: [Char] -> B.ByteString
hexToBytes str =
  fst $ B16.decode $ B.pack $ map c2w str

bytesToHex :: B.ByteString -> B.ByteString
bytesToHex =
  B16.encode

hexToBase64 :: [Char] -> B.ByteString
hexToBase64 str =
  B64.encode $ hexToBytes str

fixedXor :: [Char] -> [Char] -> B.ByteString
fixedXor inStr xorStr =
  bytesToHex $ B.pack $ map (uncurry BB.xor) $  B.zip (hexToBytes inStr) (hexToBytes xorStr)

xorByte :: Word8 -> Word8 -> Word8
xorByte =
  BB.xor

xorString :: B.ByteString -> Char -> [Char]
xorString bStr c =
  let
    cipher =
      xorBytes [c]
  in
    zipWith BB.xor (B.unpack bStr) (B.unpack cipher)
    |> map w2c

c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral

cycleCrypt :: [Char] -> [Char] -> [Char]
cycleCrypt cipherString stringToCipher =
  let
    cipher =
      xorBytes cipherString

    bytesToCipher =
      map c2w stringToCipher
--      |> B.pack

    crypted =
      zipWith BB.xor bytesToCipher (B.unpack cipher)
      |> B.pack
  in
    bytesToHex crypted
    |> B.unpack
    |> map w2c


xorBytes :: [Char] -> B.ByteString
xorBytes cipher =
  map c2w cipher
  |> B.pack
  |> B.cycle
