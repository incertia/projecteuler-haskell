module PE041 (solve041) where

import Data.List (sort)
import Math.NumberTheory.Primes (primes, unPrime)

-- abuse the fact that String = [Char]
pandigital :: Integer -> Bool
pandigital n = (== take (length (show n)) (iterate succ '1')) . sort . show $ n

-- notice that sum [1..8] = 36 and sum [1..9] = 45 so 8,9-pandigital numbers are
-- divisible by 9
solve041 :: String -> Integer
solve041 _ = maximum . filter pandigital . takeWhile (< 10^7) $ unPrime <$> primes
