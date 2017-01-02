module Utils.List
  ( pairs
  , safe_minimum
  ) where

import Data.List (tails)
pairs :: [a] -> [(a, a)]
pairs xs =
  [(x1, x2) | (x1:xs1) <- tails xs, x2 <- xs1]

safe_minimum :: Ord a => [a] -> Maybe a
safe_minimum l =
  safe_minimum' l Nothing

safe_minimum' :: Ord a => [a] -> Maybe a -> Maybe a
safe_minimum' l current_minimum =
  let
    cmp a =
      case current_minimum of
        Nothing ->
          Just a
        Just b ->
          Just (min a b)
  in
    case l of
        [] ->
          current_minimum
        (x:xs) ->
          safe_minimum' xs (cmp x)
