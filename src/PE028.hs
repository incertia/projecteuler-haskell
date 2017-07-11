module PE028 (solve028) where

solve028 :: String -> Integer
solve028 _ = foldr (+) 1 [4 * n^2 - 6 * n + 6 | n <- [3,5..1001]]
