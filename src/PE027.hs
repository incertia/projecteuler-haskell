module PE027 (solve027) where

import Control.Arrow ((&&&))
import Data.List (maximumBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Math.NumberTheory.Primes (primes, unPrime, isPrime)

countPrimes :: Integer -> Integer -> Integer
countPrimes a b = toInteger . length $ takeWhile (isJust . isPrime) (f <$> [0..])
  where f x = x^2 + a * x + b

solve027 :: String -> Integer
solve027 _ = a * b
  where vals = (,) <$> [-999..999] <*> takeWhile (<=1000) (unPrime <$> primes)
        ((a, b), _) = maximumBy (comparing snd) stuff
        stuff = (id &&& uncurry countPrimes) <$> vals
