{-# LANGUAGE OverloadedStrings #-}

module Main where

import Set1.Challenge1 as C1
import Set1.Challenge3 as C3
import Set1.Challenge4 as C4
import Set1.Challenge5 as C5
import Set1.Challenge6 as C6
import Utils.Elmify ((|>))
import qualified Stats.Frequency as Frequency
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B
import Text.Printf
import Prelude


main :: IO ()
main = do
  s1ch1
  s1ch3
  s1ch4
  s1ch5
  s1ch6
  s1ch6b
--  s1ch3a

s1ch1 :: IO ()
s1ch1 =
  do
    putStrLn "Set 1, challenge 1"
    putStrLn $ C1.challenge hexString

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
        putStrLn "Set 1, challenge 3"
        putStrLn $ show c

    resultStr =
      "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      --"1c0111001f010100061a024b53535009181c"

s1ch3 :: IO ()
s1ch3 =
  do
    putStrLn "Set 1, challenge 3"
    putStrLn doChallenge

    where
      doChallenge =
        Frequency.frequency "Cooking MC's like a pound of bacon"
        |> Frequency.fractionFrequency
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
        putStrLn "Set 1, challenge 5"
        putStrLn $ show c

s1ch5 :: IO ()
s1ch5 =
  do
    putStrLn "Set 1, challenge 5"
    putStrLn $ C5.challenge stringToCipher "ICE"
    where
      stringToCipher = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

s1ch6 ::IO ()
s1ch6 =
  do
    putStrLn "Set 1, challenge 6"
    fileContents <- B.readFile "./static/6.txt"
    mapM_ (putStrLn . showRes) $ C6.challenge fileContents

    where
      showRes (key, res) =
        (show key) ++ " : " ++ (show (B.take 64 res))

s1ch6b :: IO ()
s1ch6b =
  do
    putStrLn "Set 1, challenge 6, using the input from challenge 5"
    mapM_ (putStrLn . showRes) $ C6.challenge str2decrypt

    where
      showRes (key, res) =
        (show key) ++ " : " ++ (show (B.take 64 res))
      str2decrypt =
        "CzY3JyorLmNiLC5paSojaToqPGMkIC1iPWM0PComImMkJydlJyooKy8gQwplLixlKjEkMzplPisgJ2MMaSsgKDFlKGMmMC4nKC8="
        -- "AAsDBgMHAgcCCgILAg4GAwYCAgwCDgYJBgkCCgIDBgkDCgIKAwwGAwIEAgACDQYCAw0GAwMEAwwCCgIGAgIGAwIEAgcCBwYFAgcCCgIIAgsCDwIABAMACgYFAg4CDAYFAgoDAQIEAwMDCgYFAw4CCwIAAgcGAwAMBgkCCwIAAggDAQYFAggGAwIGAwACDgIHAggCDw=="
