module PE057 (solve057) where

import Common (conv)
import Data.Ratio (numerator, denominator)

sqrt2 :: [Integer]
sqrt2 = 1 : repeat 2

solve057 :: String -> Integer
solve057 _ = toInteger . length . filter cond . fmap (`conv` sqrt2)
           $ [1..1000]
  where cond r = length (show (numerator r)) > length (show (denominator r))
