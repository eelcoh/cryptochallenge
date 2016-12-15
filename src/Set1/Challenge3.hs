{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge3
    ( challenge
    ) where

import qualified Data.ByteString.Lazy as B

import Crypt.Attempt (Match, attempt)

challenge :: [Char] -> Match
challenge =
  attempt
