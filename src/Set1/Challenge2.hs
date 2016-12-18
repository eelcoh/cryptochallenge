{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge2
    ( challenge
    ) where

import qualified Data.ByteString.Lazy as B

import Bytes.Xor (fixedXor)
import Bytes.Utils (hexStringToByteString, byteStringToHexString)
import Utils.Elmify ((|>))

challenge :: [Char] -> [Char] -> [Char]
challenge s1 s2 =
  let
    bs1 =
      hexStringToByteString s1
    bs2 =
      hexStringToByteString s2
  in
    fixedXor bs1 bs2
    |> byteStringToHexString
