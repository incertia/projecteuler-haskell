module PE040 (solve040) where

import Data.Char (digitToInt)

-- hooray for syntactic sugar
solve040 :: String -> Integer
solve040 _ = product nums
  where champ = toInteger . digitToInt <$> concatMap show [0..]
        nums = (champ !!) <$> ((10^) <$> [0..6])
