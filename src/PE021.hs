module PE021 (solve021) where

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n - 1], n `mod` x == 0]

d :: Integer -> Integer
d = sum . divisors

amicable :: Integer -> Bool
amicable n = n /= n' && d n' == n
  where n' = d n

solve021 :: String -> Integer
solve021 _  = sum . filter amicable $ [1..9999]
