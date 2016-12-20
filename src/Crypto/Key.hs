{-# LANGUAGE OverloadedStrings #-}

module Crypto.Key
    ( search
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Function (on)

import qualified Crypto.Attempt as Attempt
import qualified Stats.HammingDistance as Hamming

import qualified Bytes.Utils as Bytes
import qualified Bytes.Xor as Xor
import Utils.Elmify ((|>))


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
-
6.  Solve each block as if it was single-character XOR. You already have code to
    do this.

7.  For each block, the single-byte XOR key that produces the best looking
    histogram is the repeating-key XOR key byte for that block.

8.  Put them together and you have the key.
-}
search :: BL.ByteString -> [(BL.ByteString, BL.ByteString)]
search bs =
  let
    -- step 1, 2 an 3 happen in findKeyLength
    keyLengths =
      findKeyLength bs
      |> take 1   -- We may have a keysize. To be sure we take the four best
                  -- matching keysizes.

    -- for all candidate keylengths, determine the key according to step 4 to 8.
    -- one of these should be the one.
    keys =
      map ((findKey bs) . fst) keyLengths

    -- here is where the actual decryption takes place.
    -- Xor.cycleXor takes a key and a ByteString, and then cycles the key as
    -- long is required (as long as the bytestring is), and 'xors' the resulting
    -- bytestring agains the given one.
    decrypt k =
      (k, Xor.cycleKey bs k)

  in
    -- returns a number of results. Of which one is presumably the one
    map decrypt keys

-- step 1-2-3
findKeyLength :: BL.ByteString -> [(Int, Double)]
findKeyLength bs =
  let
    -- step 1
    -- "2 to (say) 40"
    keyLenhgths =
      [2..40]

    -- "You could proceed perhaps with the smallest 2-3 KEYSIZE values. Or take 4
    -- KEYSIZE blocks instead of 2 and average the distances."
    numBlocks =
      12

    -- helper function: from bs, take numBlocks blocks with length l
    -- and compute the average hamming distance for these blocks
    distance l =
      blocks numBlocks l bs
      |> computeDistance l

  in
    -- compute the distance for all keylengths
    -- and sort the resuls
    map distance keyLenhgths
    |> List.sortBy (compare `on` snd)


computeDistance :: Int -> [BL.ByteString] -> (Int, Double)
computeDistance sz bss =
  let
    -- helper function to get the average of a list of Doubles
    -- uses List.genericLength to get the length of List as Double,
    -- instead of Int.
    average l =
      sum l / (List.genericLength l)

    -- make the list of bytestrings into tuple pairs
    -- so [a, b, c, d, ..] becomes [(a, b), (c, d), ..]
    -- pairs els =
    --   Split.splitEvery 2 els
    --   |> pairs'

    pairs els =
      case els of
        [] ->
          []
        a:[] ->
          []
        a:b:rest ->
          (a,b):(pairs rest)

    normalise d =
      (fromIntegral d) / (fromIntegral sz)

    -- create the pairs, feed them into the Hamming Function
    -- normalise per byte (or, more actually word8)
    -- and then compute the average
    averageDistance =
      pairs bss
      |> map Hamming.bytestrings  -- step 2: see Crypto.HammingDistance
      |> map normalise
      |> average
  in
    -- return the tuple of the size and the average distance
    (sz, averageDistance)




-- step 4-5-6-7-8
findKey :: BL.ByteString -> Int -> BL.ByteString
findKey bs sz =
  let
    -- step 5
    transposed =
      Bytes.blocks sz bs
      |> BL.transpose

    -- step 6 & 7
    key =
      map (Attempt.char . Attempt.attempt) transposed

  in
    -- step 8
    BL.pack key



-- just a utility to take the first n blocks of the blocks returned by the
-- blocks function in the Utils.Bytes package.
blocks :: Int -> Int -> BL.ByteString -> [BL.ByteString]
blocks n sz bs =
  Bytes.blocks sz bs
  |> take n
