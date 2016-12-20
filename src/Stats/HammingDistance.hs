module Stats.HammingDistance
  ( bytestrings
  , strings
  ) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Bytes.Utils (c2w)
import Utils.Elmify ((|>))
import qualified Data.Bits as BB

bytestrings :: (B.ByteString, B.ByteString) -> Int
bytestrings (bs1, bs2) =
  B.zip bs1 bs2
  |> hamming

hamming :: [(Word8, Word8)] -> Int
hamming bss =
  map (uncurry BB.xor) bss
  |> map BB.popCount
  |> sum

strings :: [Char] -> [Char] -> Int
strings str1 str2 =
  zip (map c2w str1) (map c2w str2)
  |> hamming
