{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge2
    ( challenge
    ) where

import qualified Data.ByteString.Lazy as B

import Bytes.Xor (fixedXor)
import Bytes.Utils (c2w, stringToByteString)
import Utils.Elmify ((|>))

challenge :: [Char] -> [Char] -> B.ByteString
challenge s1 s2 =
  let
    bs1 =
      stringToByteString s1
    bs2 =
      stringToByteString s2
  in
    fixedXor bs1 bs2
