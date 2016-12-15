module Stats.HammingDistance
  ( bytestrings
  , strings
  ) where

import Utils.Xor (tupleXor)
import qualified Data.ByteString.Lazy as B

bytestrings :: (B.ByteString, B.ByteString) -> Int
bytestrings (bs1, bs2) =
  B.zip bs1 bs2
  |> hamming

hamming :: [(Word8, Word8)] -> Int
hamming bss =
  map tupleXor bss
  |> map BB.popCount
  |> sum

strings :: [Char] -> [Char] -> Int
strings str1 str2 =
  zip (map c2w str1) (map c2w str2)
  |> hamming
