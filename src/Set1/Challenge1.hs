{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge1
    ( hexToBase64
    ) where

import Data.ByteString.Base64 as B64
import Data.ByteString.Base16 as B16
import qualified Data.ByteString as B

hexString =
  "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 bs =
  B64.encode $ fst $ B16.decode bs
--  print bytes
--    where
--
--      decoded16 =
--        B16.decode bs
--
--      bytes16 =
--        fst decoded16
--
--      bytes =
--        B64.encode bytes16
