module PE047 (solve047) where

import Data.List (group)
import Data.Numbers.Primes (primeFactors)

solve047 :: String -> Integer
solve047 _ = head . filter cond $ [1..]
  where cond = all cond' . take 4 . iterate succ
        cond' = (==4) . length . group . primeFactors
