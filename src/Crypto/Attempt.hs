{-# LANGUAGE OverloadedStrings #-}

module Crypto.Attempt
    ( attempt
    , Match
    , char
    , string
    , bestMatch
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16.Lazy as B16

import qualified Utils.Bytes as Bytes
import qualified Bytes.Xor as Xor
import qualified Stats.Chi as Chi
import qualified Stats.Simple as Simple

import qualified Data.List as List
import qualified Data.Char as Ch
import qualified Data.Bits as Bits

import Data.Word (Word8)

import Data.Function (on)
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
      [1..255]
      |> List.map Ch.chr -- [\001 .. \255]
      |> List.map Bytes.c2w
      -- ' ' : ['A'..'Z']

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
      Xor.cycleKeyChar bs c
      |> Bytes.byteStringToString

    score =
      Simple.simpleFrequencyWeighted decipherd

    chi =
      Chi.chi decipherd
  in
    Match c decipherd score chi
