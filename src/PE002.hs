module PE002 (solve002) where

solve002 :: String -> Integer
solve002 _ = sum . filter even $ takeWhile (<4000000) fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
