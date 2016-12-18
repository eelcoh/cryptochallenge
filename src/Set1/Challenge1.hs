{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge1
    ( challenge
    ) where

import qualified Data.ByteString.Lazy as B

import Bytes.Utils (hexstringToBase64)

challenge :: [Char] -> B.ByteString
challenge =
  hexstringToBase64
