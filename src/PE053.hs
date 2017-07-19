module PE053 (solve053) where

binom :: Integer -> Integer -> Integer
binom n k = fact n `div` (fact k * fact (n - k))
  where fact = product . enumFromTo 1

solve053 :: String -> Integer
solve053 _ = toInteger . length . filter (>10^6) $ binoms
  where binoms = binom <$> [1..100] <*> [0..100]
