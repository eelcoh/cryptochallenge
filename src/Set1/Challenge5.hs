{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge5
    ( challenge
    ) where

import qualified Utils.Hex as Hex

challenge :: [Char] -> [Char] -> [Char]
challenge cipherString stringToCipher =
  Hex.cycleCrypt cipherString stringToCipher
