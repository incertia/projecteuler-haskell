module PE048 (solve048) where

solve048 :: String -> Integer
solve048 _ = sum (zipWith (^) [1..1000] [1..1000]) `mod` 10^10
