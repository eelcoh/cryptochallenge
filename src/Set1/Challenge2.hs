{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge2
    ( challenge2
    ) where

import qualified Data.ByteString.Lazy as B

import Utils.Hex (fixedXor)

challenge2 :: [Char] -> [Char] -> B.ByteString
challenge2 inStr xorStr =
  fixedXor inStr xorStr

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
