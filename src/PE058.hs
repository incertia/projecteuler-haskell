module PE058 (solve058) where

import Data.Maybe (isJust)
import Data.Ratio ((%))
import Math.NumberTheory.Primes (isPrime)

corners :: Integer -> [Integer]
corners 1 = [1]
corners n = [(2 * n - 1)^2 - k * i | i <- [0..3]]
  where k = 2 * (n - 1)

solve058 :: String -> Integer
solve058 _ = pred . (*2) . fst . until cond f $ (0, 0)
  where f (n, c) = (n + 1, c + cpc (n + 1))
        cpc = toInteger . length . filter (isJust . isPrime) . corners
        cond (_, 0) = False
        cond (n, c) = (c % (4 * n - 3)) < (1 % 10)
