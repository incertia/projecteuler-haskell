module PE043 (solve043) where

import Data.List (permutations)
import Math.NumberTheory.Primes (primes, unPrime)

solve043 :: String -> Integer
solve043 _ = sum $ read <$> filter good things
  where things = permutations "0123456789"
        sub n x = take 3 . drop (1 + n) $ x
        good x = all (\i -> read (sub i x) `mod` unPrime (primes !! i) == 0) [0..6]
