module PE027 (solve027) where

import Control.Arrow ((&&&))
import Data.List (maximumBy)
import Math.NumberTheory.Primes (primes, isPrime)
import Data.Ord (comparing)

countPrimes :: Integer -> Integer -> Integer
countPrimes a b = toInteger . length $ takeWhile isPrime (f <$> [0..])
  where f x = x^2 + a * x + b

solve027 :: String -> Integer
solve027 _ = a * b
  where vals = (,) <$> [-999..999] <*> takeWhile (<=1000) primes
        ((a, b), _) = maximumBy (comparing snd) stuff
        stuff = (id &&& uncurry countPrimes) <$> vals
