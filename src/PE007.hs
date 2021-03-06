module PE007 (solve007) where

-- arithmoi is good
import Math.NumberTheory.Primes (unPrime, primes)

-- primes :: [Integer]
-- primes = sieve [2..]
--   where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

solve007 :: String -> Integer
solve007 _ = unPrime $ primes !! 10000
