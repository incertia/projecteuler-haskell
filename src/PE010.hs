module PE010 (solve010) where

import Math.NumberTheory.Primes (primes, unPrime)

solve010 :: String -> Integer
solve010 _ = fromIntegral . sum . takeWhile (<2000000) $ unPrime <$>primes
