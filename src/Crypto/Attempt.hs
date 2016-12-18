{-# LANGUAGE OverloadedStrings #-}

module Crypto.Attempt
    ( attempt
    , Match
    , char
    , string
    , bestMatch
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16.Lazy as B16

import qualified Utils.Bytes as Bytes
import qualified Utils.Xor as Xor
import qualified Stats.Chi as Chi
import qualified Stats.Simple as Simple

import qualified Data.List as List
import qualified Data.Char as Ch
import qualified Data.Bits as Bits

import Data.Function (on)
import Utils.Elmify ((|>))
import Text.Printf (printf)

data Match = Match
  { hexString :: [Char]
  , c :: Char
  , str :: [Char]
  , score :: Int
  , chi :: Double
  }

char :: Match -> Char
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
  c1 `compare` c2 = (score c1) `compare` (score c2)


attempt :: [Char] -> Match
attempt hexString =
  let
    chars =
      [1..255]
      |> List.map Ch.chr -- [\001 .. \255]
      -- ' ' : ['A'..'Z']

    xorBytes =
      Bytes.hexToBytes hexString

    results =
      [ decrypt hexString xorBytes c | c <- chars ]

  in
    results
    |> bestMatch

bestMatch :: [Match] -> Match
bestMatch decrypts =
  List.minimumBy (compare `on` chi) decrypts
  -- List.maximumBy (compare `on` score) decrypts

decrypt :: [Char] -> B.ByteString -> Char -> Match
decrypt hexString bytes c =
  let
    decipherd =
      Xor.xorString bytes c

    score =
      Simple.simpleFrequencyWeighted decipherd

    chi =
      Chi.chi decipherd
  in
    Match hexString c decipherd score chi
