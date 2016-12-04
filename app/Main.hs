{-# LANGUAGE OverloadedStrings #-}

module Main where

import Set1.Challenge1 as S1Ch1


main :: IO ()
main = do
  challenge1

challenge1 :: IO ()
challenge1 =
  print $ S1Ch1.hexToBase64 hexString
  where
    hexString =
      "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
