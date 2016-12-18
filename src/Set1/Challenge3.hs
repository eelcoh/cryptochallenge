{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge3
    ( challenge
    ) where

import qualified Data.ByteString.Lazy as B

import qualified Bytes.Utils as Bytes
import Crypto.AttemptBS (Match, attempt)

challenge :: [Char] -> Match
challenge =
  attempt . Bytes.hexStringToByteString
