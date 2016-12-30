module Crypto.Xor
  ( fixedXor
  , cycleKey
  , cycleKeyChar
  , search
  , attempt
  , Match
  , char
  , string
  , bestMatch
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

import qualified Data.Bits as BB

import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Function (on)

import qualified Stats.HammingDistance as Hamming
import qualified Stats.Chi as Chi
import qualified Stats.Simple as Simple

import qualified Utils.Bytes as Bytes
import Utils.Elmify ((|>))

import Text.Printf (printf)



data Match = Match
  { c :: Word8
  , str :: [Char]
  , score :: Int
  , chi :: Double
  }

char :: Match -> Word8
char ch =
  c ch

string :: Match -> [Char]
string ch =
  str ch

instance Show Match where
  show t = (show (c t))  ++ " : " ++ (show (score t)) ++ " : " ++ (printf "%.2f" (chi t)) ++ " : " ++ (show (str t))

instance Eq Match where
  c1 == c2 =
      and
        [ (c c1) == (c c2)
        , (str c1) == (str c2)
        , (score c1) == (score c2)
        ]

instance Ord Match where
  c1 `compare` c2 = (chi c1) `compare` (chi c2)

attemptFromChars :: [Char] -> Match
attemptFromChars hexString =
  Bytes.hexStringToByteString hexString
  |> attempt

attempt :: B.ByteString -> Match
attempt bs =
  let
    chars =
      Bytes.all_chars

    results =
      [ decrypt bs c | c <- chars ]

  in
    results
    |> bestMatch

bestMatch :: [Match] -> Match
bestMatch decrypts =
  List.minimumBy (compare `on` chi) decrypts

decrypt :: B.ByteString -> Word8 -> Match
decrypt bs c =
  let
    decipherd =
      cycleKeyChar bs c
      |> Bytes.byteStringToString

    score =
      Simple.simpleFrequencyWeighted decipherd

    chi =
      Chi.chi decipherd
  in
    Match c decipherd score chi


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
search :: B.ByteString -> [(B.ByteString, B.ByteString)]
search bs =
  let
    -- step 1, 2 an 3 happen in findKeyLength
    keyLengths =
      findKeyLength bs
      |> take 2   -- We may have a keysize. To be sure we take the best
                  -- matching keysizes.

    -- for all candidate keylengths, determine the key according to step 4 to 8.
    -- one of these should be the one.
    keys =
      map ((findKey bs) . fst) keyLengths

    -- here is where the actual decryption takes place.
    -- cycleKey takes a key and a ByteString, and then cycles the key as long
    -- is required (as long as the bytestring is), and 'xors' the resulting
    -- bytestring agains the given one.
    decrypt k =
      (k, cycleKey bs k)

  in
    -- returns a number of results. Of which one is presumably the one
    map decrypt keys

-- step 1-2-3
findKeyLength :: B.ByteString -> [(Int, Double)]
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


computeDistance :: Int -> [B.ByteString] -> (Int, Double)
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
findKey :: B.ByteString -> Int -> B.ByteString
findKey bs sz =
  let
    -- step 5
    transposed =
      Bytes.blocks sz bs
      |> B.transpose

    -- step 6 & 7
    key =
      map (char . attempt) transposed

  in
    -- step 8
    B.pack key



-- just a utility to take the first n blocks of the blocks returned by the
-- blocks function in the Utils.Bytes package.
blocks :: Int -> Int -> B.ByteString -> [B.ByteString]
blocks n sz bs =
  Bytes.blocks sz bs
  |> take n

-- XOR two bytestrings of the same length
-- uses zipWith to take one byte of each ByteString and apply the xor function on those two
-- and do that for all bytes
fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor bs1 bs2 =
  B.zipWith BB.xor bs1 bs2
  |> B.pack

-- a wrapper for the cycleKey function. cycleKey takes a ByteString, whereas
-- cycleKeyChar takes single Word8. So we convert that into a ByteString first
cycleKeyChar :: B.ByteString -> Word8 -> B.ByteString
cycleKeyChar bs w =
  B.pack [w]
  |> cycleKey bs

-- cycleKey makes use of the cycle function of Data.ByteString.Lazy
-- it takes a ByteString and creates a new infinite ByteString out of it
-- cycling the key indefinately. Since it is a lazy ByteString, we only use
-- it as far as needed, which is the length of the incoming ByteString (bs)
-- So these two are XORred against eachother, just like the fixedXor function
-- does for two ByteStrings of the same length.
cycleKey :: B.ByteString -> B.ByteString -> B.ByteString
cycleKey bs key =
  let
    cycledKey =
      BL.fromStrict key
      |>BL.cycle

  in
    BL.zipWith BB.xor cycledKey (BL.fromStrict bs)
    |> BL.pack
    |> BL.toStrict
