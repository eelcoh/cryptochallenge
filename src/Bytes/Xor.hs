module Bytes.Xor
  ( fixedXor
  , cycleKey
  , cycleKeyChar
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Word (Word8)

import Utils.Elmify ((|>))
import Utils.Bytes (c2w, w2c, bytesToHex, hexToBytes)
import qualified Data.Bits as BB


fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor bs1 bs2 =
  B.zipWith BB.xor bs1 bs2
  |> B.pack


cycleKeyChar :: B.ByteString -> Word8 -> B.ByteString
cycleKeyChar bs w =
  B.pack [w]
  |> cycleKey bs


cycleKey :: B.ByteString -> B.ByteString -> B.ByteString
cycleKey bs key =
  let
    cycledKey =
      B.cycle key
  in
    B.zipWith BB.xor cycledKey bs
    |> B.pack
