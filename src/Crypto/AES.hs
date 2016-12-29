{-# LANGUAGE OverloadedStrings #-}

module Crypto.AES
    ( initAES128
    , cryptKey128
    , decryptKey128
    , detect
    , detect_oracle
    , cbcEncrypt
    , cbcDecrypt
    , encryption_oracle
    , encryption_oracles
    , Mode
    ) where

import qualified Data.ByteString as B

-- import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Cipher as C

import System.Random

import Bytes.Utils as Bytes
import Bytes.Xor as Xor
import Utils.List (pairs)
import Data.List (last)
import Data.Word (Word8)

import Stats.HammingDistance as Hamming

import Utils.Elmify ((|>))

data Mode
  = EBC
  | CBC
  deriving (Show, Enum, Bounded)

instance Random Mode where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- the bytestring need to have a length of 16 bytes
-- otherwise the simplified error handling will raise an exception.
initAES128 :: B.ByteString -> C.AES128
initAES128 = either (error . show) C.cipherInit . C.makeKey

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
initAES256 :: B.ByteString -> C.AES256
initAES256 = either (error . show) C.cipherInit . C.makeKey


-- ECB
-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
cryptKey256 key msg = C.ecbEncrypt ctx msg
  where ctx = initAES256 key

decryptKey128 key msg = C.ecbDecrypt ctx msg
  where ctx = initAES128 key

cryptKey128 key msg = C.ecbEncrypt ctx msg
  where ctx = initAES128 key

getRandoms :: StdGen -> Int -> (B.ByteString, StdGen)
getRandoms g keysz =
  let
    createRandoms =
      iterate (\rg -> random (snd rg) :: (Word8, StdGen))

    toByteString rgs =
      map fst rgs
      |> B.pack
      |> (flip (,)) (last rgs |> snd)

  in
    createRandoms (0x00, g)
    |> take keysz
    |> toByteString


encryption_oracles :: StdGen -> Int -> B.ByteString -> [B.ByteString]
encryption_oracles g num stringToCipher =
  let
    oracle f =
      iterate (\rg -> (f (snd rg))) (stringToCipher, g)
    encr =
      (flip encryption_oracle) stringToCipher
  in
    oracle encr
    |> take num
    |> map fst
    |> tail


encryption_oracle :: StdGen -> B.ByteString -> (B.ByteString, StdGen)
encryption_oracle g0 str =
  let
    keysz =
      16

    (key, g1) =
      getRandoms g0 keysz

    (numBefore, g2) =
      randomR (5, 10) g1

    (numAfter, g3) =
      randomR (5, 10) g2

    (padBefore, g4) =
      getRandoms g3 numBefore

    (padAfter, g5) =
      getRandoms g4 numAfter

    (iv, g6) =
      getRandoms g5 keysz

    (mode, g7) =
      random g6 :: (Mode, StdGen)

    stringToCipher =
      B.concat [padBefore, str, padAfter]

  in
    case mode of
      CBC ->
        (cbcEncrypt key iv stringToCipher, g7)
      EBC ->
        (ecbEncrypt key stringToCipher, g7)


detect_oracle :: Int -> B.ByteString -> Mode
detect_oracle keysz str =
  let
    (dst, _) =
      detect keysz str
  in
    if dst == 0
      then
        EBC
      else
        CBC

detect :: Int -> B.ByteString -> (Int, B.ByteString)
detect key str =
  Bytes.blocks key str
  |> pairs
  |> map Hamming.bytestrings
  |> minimum
  |> ((flip (,)) str)



ecbEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbEncrypt key stringToCipher =
  let
    ctx =
      initAES128 key

    keysz =
      B.length key

  in
    Bytes.blocks keysz stringToCipher
    |> map ((C.ecbDecrypt ctx) . (Bytes.pad keysz))
    |> B.concat

-- https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#CBC
cbcEncrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbcEncrypt key iv stringToCipher =
  let
    keysz =
      B.length key

    ctx =
      initAES128 key

    blocksToCipher =
      Bytes.blocks keysz stringToCipher
      |> map (Bytes.pad keysz)

    lengthOK x =
      B.length x == 16

  in
    cbc ctx iv blocksToCipher
    |> B.concat


cbc :: C.AES128 -> B.ByteString -> [B.ByteString] -> [B.ByteString]
cbc ctx iv blocksToCipher =
  case blocksToCipher of
    [] ->
      []
    (x:xs) ->
      let
        firstBlock =
          Xor.fixedXor iv x
          |> C.ecbEncrypt ctx

        nextBlocks =
          cbc ctx firstBlock xs

      in
        firstBlock:nextBlocks

cbcDecrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbcDecrypt key iv stringToDecipher =
  let
    keysz =
      B.length key

    ctx =
      initAES128 key

    blocksToDecipher =
      Bytes.blocks keysz stringToDecipher

    blocksDecipherd =
      map (C.ecbDecrypt ctx) blocksToDecipher

    blocksToXor =
      (iv, (head blocksDecipherd)) : (zip blocksToDecipher (tail blocksDecipherd))

  in
    map (uncurry Xor.fixedXor) blocksToXor
    |> B.concat
