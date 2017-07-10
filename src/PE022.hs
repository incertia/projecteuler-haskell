module PE022 (solve022) where

import Data.Char (ord)
import Data.List (sort)

solve022 :: String -> Integer
solve022 input = sum scores
  where score :: String -> Integer
        score = foldr (\c s -> toInteger (ord c - ord 'A') + 1 + s) 0
        names = sort . read $ "[" ++ input ++ "]"
        scores = zipWith (*) (score <$> names) [1..]
