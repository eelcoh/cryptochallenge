{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge6
    ( challenge
    ) where

import Crypto.Key as Key
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import Utils.Elmify ((|>))
import Bytes.Utils (c2w)

challenge :: B.ByteString -> [(B.ByteString, B.ByteString)]
challenge contents =
  -- Key.search contents
  -- |> map (\ (a, b) -> (a, B64.decodeLenient b))
  B64.decodeLenient contents
  |> Key.search

{-

1.  Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.

2.  Write a function to compute the edit distance/Hamming distance between two
    strings. The Hamming distance is just the number of differing bits. The
    distance between:

      this is a test

    and

      wokka wokka!!!

    is 37. Make sure your code agrees before you proceed.

3.  For each KEYSIZE, take the first KEYSIZE worth of bytes, and the second
    KEYSIZE worth of bytes, and find the edit distance between them. Normalize
    this result by dividing by KEYSIZE.
    The KEYSIZE with the smallest normalized edit distance is probably the key.
    You could proceed perhaps with the smallest 2-3 KEYSIZE values. Or take 4
    KEYSIZE blocks instead of 2 and average the distances.

4.  Now that you probably know the KEYSIZE: break the ciphertext into blocks of
    KEYSIZE length.

5.  Now transpose the blocks: make a block that is the first byte of every block,
    and a block that is the second byte of every block, and so on.

6.  Solve each block as if it was single-character XOR. You already have code to
    do this.

7.  For each block, the single-byte XOR key that produces the best looking
    histogram is the repeating-key XOR key byte for that block. Put them
    together and you have the key.

-}
