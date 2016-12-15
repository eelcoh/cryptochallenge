{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge2
    ( challenge
    ) where

import qualified Data.ByteString.Lazy as B

import Utils.Bytes (fixedXor)

challenge :: [Char] -> [Char] -> B.ByteString
challenge =
  fixedXor
