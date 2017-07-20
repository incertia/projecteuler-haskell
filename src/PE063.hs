module PE063 (solve063) where

solve063 :: String -> Integer
solve063 _ = toInteger . length . filter cond $ (,) <$> [1..9] <*> [1..maxPower]
  where maxPower = last . takeWhile (curry cond 9) $ [1..]
        cond (n, k) = length (show (n^k)) == k
