{-# LANGUAGE OverloadedStrings #-}

module Set1.Challenge3
    ( challenge
    , Challenge
    , char
    , string
    , bestMatch
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16.Lazy as B16

import qualified Utils.Hex as Hex
import qualified Utils.Stats as Stats

import qualified Data.List as List
import qualified Data.Char as Ch
import qualified Data.Bits as Bits

import Data.Function (on)
import Utils.Elmify ((|>))
import Text.Printf (printf)

data Challenge = Challenge
  { hexString :: [Char]
  , c :: Char
  , str :: [Char]
  , score :: Int
  , chi :: Double
  }

char :: Challenge -> Char
char ch =
  c ch

string :: Challenge -> [Char]
string ch =
  str ch

instance Show Challenge where
  show t = (show (c t))  ++ " : " ++ (show (score t)) ++ " : " ++ (printf "%.2f" (chi t)) ++ " : " ++ (show (str t))

instance Eq Challenge where
  c1 == c2 =
      and
        [ (c c1) == (c c2)
        , (str c1) == (str c2)
        , (score c1) == (score c2)
        ]

instance Ord Challenge where
  c1 `compare` c2 = (score c1) `compare` (score c2)

challenge :: [Char] -> Challenge
challenge hexString =
  let
    chars =
      [1..255]
      |> List.map Ch.chr -- [\001 .. \255]
      -- ' ' : ['A'..'Z']

    xorBytes =
      Hex.hexToBytes hexString

    results =
      [ create hexString xorBytes c | c <- chars ]

  in
    results
    |> bestMatch

bestMatch :: [Challenge] -> Challenge
bestMatch challenges =
  List.minimumBy (compare `on` chi) challenges
  -- List.maximumBy (compare `on` score) challenges

create :: [Char] -> B.ByteString -> Char -> Challenge
create hexString bytes c =
  let
    decipherd =
      Hex.xorString bytes c

    score =
      Stats.simpleFrequencyWeighted decipherd

    chi =
      Stats.chi decipherd
  in
    Challenge hexString c decipherd score chi
