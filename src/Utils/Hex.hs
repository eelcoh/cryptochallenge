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
    , blocks
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
import Data.Function
import Utils.Elmify
import GHC.Int (Int64)
import qualified Data.List as List
import qualified Data.List.Split as Split

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

hammingDistanceBS :: B.ByteString -> B.ByteString -> Int
hammingDistanceBS bs1 bs2 =
  B.zip bs1 bs2
  |> hammingDistance

hammingDistance :: [(Word8, Word8)] -> Int
hammingDistance bss =
  map (uncurry BB.xor) bss
  |> map BB.popCount
  |> sum

hammingDistanceChar :: [Char] -> [Char] -> Int
hammingDistanceChar str1 str2 =
  zip (map c2w str1) (map c2w str2)
  |> hammingDistance

blocks :: Int -> Int -> B.ByteString -> [B.ByteString]
blocks n sz bs =
  let
    s =
      fromIntegral sz :: GHC.Int.Int64
  in
    case ((n <= 0), ((B.length bs) < s)) of
      (True, _) ->
        []

      (False, True) ->
        [bs]

      (False, False) ->
        let
          (h, t) =
            B.splitAt s bs
        in
          h : blocks (n - 1) sz t

computeKeyLength :: Int -> [B.ByteString] -> (Int, Double)
computeKeyLength sz bss =
  let
    average l =
      sum l / (List.genericLength l)

    pairs els =
      Split.splitEvery 2 els
      |> map (\[bs1, bs2] -> (bs1, bs2))

    computed =
      pairs bss
      |> map (uncurry hammingDistanceBS)
      |> map (\d -> (fromIntegral d) / (fromIntegral sz))
      |> average
  in
    (sz, computed)



findKeyLength :: B.ByteString -> [(Int, Double)]
findKeyLength bs =
  let
    keyLenhgths =
      [2..40]

    numBlocks =
      4

    blks l =
      blocks numBlocks l bs

  in
    map (\kl -> (kl, blks kl)) keyLenhgths
    |> map (uncurry computeKeyLength)
    |> List.sortBy (compare `on` snd)
