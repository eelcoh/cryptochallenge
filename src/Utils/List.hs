module Utils.List
  ( pairs
  ) where

import Data.List (tails)
pairs :: [a] -> [(a, a)]
pairs xs =
  [(x1, x2) | (x1:xs1) <- tails xs, x2 <- xs1]
