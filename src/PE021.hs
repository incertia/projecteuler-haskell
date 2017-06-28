module PE021 (solve021) where

-- TODO: this doesn't work yet
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n - 1], n `mod` x == 0]

d :: Integer -> Integer
d = sum . divisors

amicable :: Integer -> Bool
amicable n = d n' == n
  where n' = d n

solve021 :: IO ()
solve021 = print . sum . filter amicable $ [1..9999]
