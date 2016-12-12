module Utils.Elmify
  ( (|>)
  , (<|)
  ) where

(|>) = flip ($)
(<|) = ($)
