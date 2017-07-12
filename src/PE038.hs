module PE038 (solve038) where

import Data.List (sort)

pandigital :: [Integer] -> Bool
pandigital = (=="123456789") . sort . concatMap show

-- we could do math to concatenate numbers but read . concatMap show is a pretty
-- easy way to cheat the system at a /slight/ performance hit
panprod :: Integer -> [Integer]
panprod n = read . concatMap show <$> filter pandigital multiples
  where multiples = [(n*) <$> [1..k] | k <- [2..9]]

solve038 :: String -> Integer
solve038 _ = maximum . concatMap panprod $ [1..9999]
