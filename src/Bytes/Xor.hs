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

-- XOR two bytestrings of the same length
-- uses zipWith to take one byte of each ByteString and apply the xor function on those two
-- and do that for all bytes
fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor bs1 bs2 =
  B.zipWith BB.xor bs1 bs2
  |> B.pack

-- a wrapper for the cycleKey function. cycleKey takes a ByteString, whereas
-- cycleKeyChar takes single Word8. So we convert that into a ByteString first
cycleKeyChar :: B.ByteString -> Word8 -> B.ByteString
cycleKeyChar bs w =
  B.pack [w]
  |> cycleKey bs

-- cycleKey makes use of the cycle function of Data.ByteString.Lazy
-- it takes a ByteString and creates a new infinite ByteString out of it
-- cycling the key indefinately. Since it is a lazy ByteString, we only use
-- it as far as needed, which is the length of the incoming ByteString (bs)
-- So these two are XORred against eachother, just like the fixedXor function
-- does for two ByteStrings of the same length.
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
