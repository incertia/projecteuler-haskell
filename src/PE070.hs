module PE070 (solve070) where

import Data.List (minimumBy, sort)
import Data.Ratio ((%))
import Data.Ord (comparing)
import Math.NumberTheory.Powers (integerSquareRoot)
import Math.NumberTheory.Primes (primes)

-- notice that n / phi(n) = \product_{p \mid n} \frac{p}{p - 1}
-- an odd number of primes won't work for some provable reason
-- we have n = p_1^e_1 ... p_k^e_k for k = 2j
-- if we take two large primes close to sqrt(10^7), we can minimize the product
-- if we take four primes, the primes must also be smaller so the product will
-- be larger, which is not ideal
-- so we cheese the solution by just taking products of two primes close to the
-- square root of 10^7
-- exhaustive search takes a little less than 90 seconds which feels really slow

solve070 :: String -> Integer
solve070 _ = fst . minimumBy (comparing snd) $ nums
  where nums = [(p * q, totientRatio p q) | p <- pl,
                                            q <- dropWhile (<p) pl,
                                            p * q < 10^7,
                                            totientPerm p q]
        totientPerm p q = sort (show (p * q)) == sort (show ((p - 1) * (q - 1)))
        totientRatio p q = (p * q) % ((p - 1) * (q - 1))
        pl = filter ((==2) . (`mod` 3)) . takeWhile (<5000) $ primes
