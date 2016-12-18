{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge5
    ( challenge
    ) where

import qualified Utils.Xor as Xor

challenge :: [Char] -> [Char] -> [Char]
challenge  =
  Xor.cycleXor
