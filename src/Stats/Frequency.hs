module Stats.Frequency
  ( FrequencyMap
  , empty
  , english
  , frequency
  , fractionFrequency
  ) where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map

import Utils.Elmify


type FrequencyMap = Map.Map Char Double

empty :: FrequencyMap
empty = Map.fromList
  [ ('A', 0.0)
  , ('B', 0.0)
  , ('C', 0.0)
  , ('D', 0.0)
  , ('E', 0.0)
  , ('F', 0.0)
  , ('G', 0.0)
  , ('H', 0.0)
  , ('I', 0.0)
  , ('J', 0.0)
  , ('K', 0.0)
  , ('L', 0.0)
  , ('M', 0.0)
  , ('N', 0.0)
  , ('O', 0.0)
  , ('P', 0.0)
  , ('Q', 0.0)
  , ('R', 0.0)
  , ('S', 0.0)
  , ('T', 0.0)
  , ('U', 0.0)
  , ('V', 0.0)
  , ('W', 0.0)
  , ('X', 0.0)
  , ('Y', 0.0)
  , ('Z', 0.0)
  , (' ', 0.0)
  ]

english :: FrequencyMap
english =
  let
    normalChars =
      Map.fromList
        [ ('A', 0.0651738)
        , ('B', 0.0124248)
        , ('C', 0.0217339)
        , ('D', 0.0349835)
        , ('E', 0.1041442)
        , ('F', 0.0197881)
        , ('G', 0.0158610)
        , ('H', 0.0492888)
        , ('I', 0.0558094)
        , ('J', 0.0009033)
        , ('K', 0.0050529)
        , ('L', 0.0331490)
        , ('M', 0.0202124)
        , ('N', 0.0564513)
        , ('O', 0.0596302)
        , ('P', 0.0137645)
        , ('Q', 0.0008606)
        , ('R', 0.0497563)
        , ('S', 0.0515760)
        , ('T', 0.0729357)
        , ('U', 0.0225134)
        , ('V', 0.0082903)
        , ('W', 0.0171272)
        , ('X', 0.0013692)
        , ('Y', 0.0145984)
        , ('Z', 0.0007836)
        , (' ', 0.1918182)
        , ('\n', 0.0400000)
        , ('\039', 0.0400000) -- '
        ]

    allChars =
      [1..255]
      |> List.map (\c -> (Char.chr c, 0.0000001))
      |> Map.fromList
  in
    Map.union normalChars allChars


frequency :: [Char] -> [(Char, Int)]
frequency str =
  let
    sortedChars =
      map Char.toUpper str
      |> List.sort
    uniqueChars =
      List.nub sortedChars
    groupedChars =
      List.group sortedChars
      |> (List.map List.length)
  in
    List.zip uniqueChars groupedChars

fractionFrequency :: [(Char, Int)] -> [(Char, Double)]
fractionFrequency freqs =
  let
    total =
      List.map snd freqs
      |> sum
    fraction (a, freq) =
      (a, (fromIntegral freq / fromIntegral total ))
  in
    List.map fraction freqs
