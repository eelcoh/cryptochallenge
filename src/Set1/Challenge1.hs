{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge1
    ( challenge
    ) where

import qualified Data.ByteString.Lazy as B

import Utils.Bytes (hexToBase64)

challenge :: [Char] -> B.ByteString
challenge =
  hexToBase64
