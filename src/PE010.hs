module PE010 (solve010) where

import Data.Numbers.Primes (primes)

solve010 :: String -> Integer
solve010 _ = fromIntegral . sum $ takeWhile (<2000000) primes
