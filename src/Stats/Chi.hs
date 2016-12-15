module Stats.Chi
  ( chi
  ) where

import qualified Data.Map as Map
import qualified Stats.Frequency as Frequency

chi :: [Char] -> Double
chi str =
  let
    observedFreqTable =
      frequency str
      |> fractionFrequency
      |> Map.fromList
      |> (flip Map.union) Frequency.empty

    frequencyTuples =
      Map.intersectionWith (,) observedFreqTable englishFreqTable
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
