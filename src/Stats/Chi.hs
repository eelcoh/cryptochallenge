module Stats.Chi
  ( chi
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Stats.Frequency as Frequency

import Utils.Elmify ((|>))

chi :: [Char] -> Double
chi str =
  let
    observedFreqTable =
      Frequency.frequency str
      |> Frequency.fractionFrequency
      |> Map.fromList
      |> (flip Map.union) Frequency.empty

    frequencyTuples =
      Map.intersectionWith (,) observedFreqTable Frequency.english
      |> Map.elems
  in
    chiSqr frequencyTuples


chiSqr :: [(Double, Double)] -> Double
chiSqr res =
  let
    term (observation, expectation) =
      (observation - expectation) ^ 2 / expectation
  in
    List.map term res
    |> sum
