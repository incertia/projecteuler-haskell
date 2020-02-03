module PE069 (solve069) where

import Math.NumberTheory.Primes (primes, unPrime)

solve069 :: String -> Integer
solve069 _ = last . takeWhile (<10^6) . scanl (*) 1 $ unPrime <$> primes
