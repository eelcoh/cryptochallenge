module Bytes.Xor
  ( fixedXor
  , cycleKey
  , cycleKeyChar
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

import Utils.Elmify ((|>))
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
      BL.fromStrict key
      |>BL.cycle

  in
    BL.zipWith BB.xor cycledKey (BL.fromStrict bs)
    |> BL.pack
    |> BL.toStrict
