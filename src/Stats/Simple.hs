{-# LANGUAGE OverloadedStrings #-}

module Stats.Simple
  ( simpleFrequency
  , simpleFrequencyWeighted
  ) where

import Utils.Elmify ((|>))

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

frequentChars :: [Char]
frequentChars = "ETAOIN SHRDLU"


simpleFrequency :: [Char] -> Int
simpleFrequency str =
  map Char.toUpper str
  |> List.filter ((flip elem) frequentChars)
  |> List.length


simpleFrequencyWeighted :: [Char] -> Int
simpleFrequencyWeighted str =
  map Char.toUpper str
  |> Maybe.mapMaybe ((flip Map.lookup) frequentCharsWeight)
  |> sum


frequentCharsWeight :: Map.Map Char Int
frequentCharsWeight =
  let
    l =
      length frequentChars
    weights =
      [1..l]
      |> reverse
  in
    zip frequentChars weights
    |> Map.fromList
