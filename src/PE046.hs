module PE046 (solve046) where

import Common (hasiroot)
import Data.Numbers.Primes (primes, isPrime)

gc :: Integer -> Bool
gc n = any hasiroot nums
  where nums = [ n' `div` 2 | x <- primes',
                              let n' = n - x,
                              n' `mod` 2 == 0 ]
        primes' = takeWhile (<n) primes

solve046 :: String -> Integer
solve046 _ = head . filter (not . gc) $ [n' | n <- [1..],
                                              let n' = 2 * n + 1,
                                              not (isPrime n') ]
