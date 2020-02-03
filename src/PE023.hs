module PE023 (solve023) where

import Data.Array (listArray, (!))
import Data.List (group)
import Math.NumberTheory.Primes (primes, factorise, unPrime)

d :: Integer -> Integer
d n = product [(unPrime p ^ (k + 1) - 1) `div` (unPrime p - 1) | (p,k) <- factorise n]

ab :: Integer -> Bool
ab n = d n - n > n

solve023 :: String -> Integer
solve023 _ = sum [1..n] - sum absums
  where absums = [x | x <- [1..n],
                      let a = (x-) <$> takeWhile (<= x `div` 2) abs,
                      any (abv !) a
                 ]
        abs = filter (abv !) [1..n]
        abv = listArray (1, n) $ ab <$> [1..n]
        n = 28123
