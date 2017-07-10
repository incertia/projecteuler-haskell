module PE005 (solve005) where

solve005 :: String -> Integer
solve005 _ = foldr lcm 1 [1..20]
