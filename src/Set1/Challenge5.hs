{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge5
    ( challenge
    ) where

import qualified Crypto.Xor (cycleXor)

challenge :: [Char] -> [Char] -> [Char]
challenge  =
  cycleXor
