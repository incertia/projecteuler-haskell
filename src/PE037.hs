module PE037 (solve037) where

import Data.Numbers.Primes (primes, isPrime)

truncatable :: Integer -> Bool
truncatable n = ltrunc n && rtrunc n
  where ltrunc n = isPrime n && (n < 10 || ltrunc (n `mod` 10 ^ (digits n - 1)))
        rtrunc n = isPrime n && (n < 10 || rtrunc (n `div` 10))
        digits = length . show

solve037 :: String -> Integer
solve037 _ = sum . take 11 . filter truncatable . filter (>7) $ primes
