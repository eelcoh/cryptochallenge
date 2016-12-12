{-# LANGUAGE OverloadedStrings #-}

module Main where

import Set1.Challenge1 as S1Ch1
import Set1.Challenge3 as C3
import Set1.Challenge4 as C4
import Set1.Challenge5 as C5
import Utils.Hex as Hex
import Utils.Elmify
import qualified Utils.Stats as Stats
--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Text.Printf
import Prelude

main :: IO ()
main = do
  s1ch1
  s1ch3
  s1ch4
  s1ch5
--  s1ch3a

s1ch1 :: IO ()
s1ch1 =
  print $ S1Ch1.challenge hexString

  where
    hexString =
      "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"


s1ch3 :: IO ()
s1ch3 =
  showRes res
  where
    res =
      C3.challenge resultStr
--      |> C3.bestMatch

    showRes c =
      do
        putStrLn $ show c

    resultStr =
      "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      --"1c0111001f010100061a024b53535009181c"

s1ch3a :: IO ()
s1ch3a =
  Stats.frequency "Cooking MC's like a pound of bacon"
  |> Stats.fractionFrequency
  |> show
  |> print

s1ch4 :: IO ()
s1ch4 =
  showRes res
  where
    res =
      C4.challenge

    showRes c =
      do
        putStrLn $ show c

s1ch5 :: IO ()
s1ch5 =
  do
    putStrLn $ C5.challenge "ICE" stringToCipher
    where
      stringToCipher = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
