{-# LANGUAGE OverloadedStrings #-}

module Utils.Strings
  ( substrings
  , blocks
  ) where

import Utils.Elmify ((|>))

import Data.List
import qualified Data.List.Split as Split

import Data.Function

-- chop into bytestrings of sz bytes
blocks :: Int -> [Char] -> [[Char]]
blocks sz s =
  Split.chunksOf (fromIntegral sz) s

substrings xs ys =
  let
    f xs ys =
      scanl g [] $ zip xs ys

    g z (x, y) =
      if x == y
        then
          z ++ [x]
        else
          []

    left =
      [f xs' ys | xs' <- tails xs]

    right =
      [f xs ys' | ys' <- drop 1 $ tails ys]
  in
    maximumBy (compare `on` length) . concat $  left ++ right
