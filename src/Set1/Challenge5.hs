{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge5
    ( challenge
    ) where

import Bytes.Utils (stringToByteString, byteStringToHexString )
import qualified Bytes.Xor as Xor
import Utils.Elmify ((|>))

challenge :: [Char] -> [Char] -> [Char]
challenge stringToCipher key =
  Xor.cycleKey (stringToByteString stringToCipher) (stringToByteString key)
  |> byteStringToHexString
