{-# LANGUAGE OverloadedStrings #-}

module Main where

import Set1.Challenges as S1

import Utils.Elmify ((|>))
import qualified Stats.Frequency as Frequency
--import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Text.Printf
import Prelude


main :: IO ()
main = do
  s1ch1
  s1ch2
  s1ch3
  s1ch4
  s1ch5
  s1ch6
  s1ch6b
  s1ch7
  s1ch8

s1ch1 :: IO ()
s1ch1 =
  do
    putStrLn "Set 1, challenge 1"
    putStrLn $ show $ S1.challenge1 hexString
    putStrLn ""

    where
      hexString =
        "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"


s1ch2 :: IO ()
s1ch2 =
  showRes res

  where
    res =
      S1.challenge2 "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"

    showRes c =
      do
        putStrLn "Set 1, challenge 2"
        putStrLn $ show c
        putStrLn ""

s1ch3 :: IO ()
s1ch3 =
  showRes res
  where
    res =
      S1.challenge3 resultStr
--      |> C3.bestMatch

    showRes c =
      do
        putStrLn "Set 1, challenge 3"
        putStrLn $ show c
        putStrLn ""

    resultStr =
      "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      --"1c0111001f010100061a024b53535009181c"


s1ch4 :: IO ()
s1ch4 =
  showRes res
  where
    res =
      S1.challenge4

    showRes c =
      do
        putStrLn "Set 1, challenge 4"
        putStrLn $ show c
        putStrLn ""

s1ch5 :: IO ()
s1ch5 =
  do
    putStrLn "Set 1, challenge 5"
    putStrLn $ S1.challenge5 stringToCipher "ICE"
    putStrLn ""

    where
      stringToCipher = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

s1ch6 ::IO ()
s1ch6 =
  do
    putStrLn "Set 1, challenge 6"
    fileContents <- B.readFile "./static/6.txt"
    mapM_ (putStrLn . showRes) $ S1.challenge6 fileContents
    putStrLn ""

    where
      showRes (key, res) =
        (show key) ++ " : " ++ (show (B.take 64 res))

s1ch6b :: IO ()
s1ch6b =
  do
    putStrLn "Set 1, challenge 6, using the input from challenge 5"
    mapM_ (putStrLn . showRes) $ S1.challenge6 str2decrypt
    putStrLn ""

    where
      showRes (key, res) =
        (show key) ++ " : " ++ (show (B.take 64 res))
      str2decrypt =
        "CzY3JyorLmNiLC5paSojaToqPGMkIC1iPWM0PComImMkJydlJyooKy8gQwplLixlKjEkMzplPisgJ2MMaSsgKDFlKGMmMC4nKC8="
        -- "AAsDBgMHAgcCCgILAg4GAwYCAgwCDgYJBgkCCgIDBgkDCgIKAwwGAwIEAgACDQYCAw0GAwMEAwwCCgIGAgIGAwIEAgcCBwYFAgcCCgIIAgsCDwIABAMACgYFAg4CDAYFAgoDAQIEAwMDCgYFAw4CCwIAAgcGAwAMBgkCCwIAAggDAQYFAggGAwIGAwACDgIHAggCDw=="

s1ch7 ::IO ()
s1ch7 =
  do
    putStrLn "Set 1, challenge 7"
    fileContents <- B.readFile "./static/7.txt"
    putStrLn $ showRes $ S1.challenge7 "YELLOW SUBMARINE" fileContents
    putStrLn ""

    where
      showRes res =
        show (B.take 64 res)

s1ch8 ::IO ()
s1ch8 =
  do
    putStrLn "Set 1, challenge 7"
    fileContents <- readFile "./static/8.txt"
    mapM_ (putStrLn . showRes) $ S1.challenge8 "YELLOW SUBMARINE" $ lines fileContents
    putStrLn ""

    where
      showRes res =
        show (B.take 64 res)
