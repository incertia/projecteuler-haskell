module PE050 (solve050) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Math.NumberTheory.Primes (primes, isPrime)

-- the sum of the last 21 elements of pl is the largest consecutive prime sum
-- of 21 elements under one million
pl :: [Integer]
pl = takeWhile (<=ub) primes
  where ub = until (\x -> 21 * x + 420 > 10^6) succ 0 + 40

-- take advantage of laziness with takeWhile
-- using Int as the counter saves like ~40ms
sums :: [Integer] -> [(Integer, Int)]
sums [] = []
sums v = filter (isPrime . fst) sums' ++ sums (tail v)
  where sums' = takeWhile ((<10^6) . fst) . scanl accum (0, 0) $ v
        accum (s, c) p = (s + p, c + 1)

solve050 :: String -> Integer
solve050 _ = fst . maximumBy (comparing snd) . sums $ pl
