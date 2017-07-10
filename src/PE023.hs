module PE023 (solve023) where

import Data.MemoCombinators (integral)
import Data.List (group)
import Data.Numbers.Primes (primes)

pf :: Integer -> [Integer]
pf = integral pf'
  where pf' 1 = []
        pf' n = p : pf (n `div` p)
          where p = head [x | x <- primes, n `mod` x == 0]

d :: Integer -> Integer
d n = product [let k = length pl + 1 in (p^k - 1) `div` (p - 1) | pl@(p:_) <- group (pf n)] - n

ab :: Integer -> Bool
ab n = d n > n

solve023 :: String -> Integer
solve023 _ = sum [x | x <- l, x `notElem` absums]
  where absums = sum <$> combos
        combos = filter (\x -> head x < head (tail x)) $ mapM (const nums) [1..2]
        nums = filter ab l
        l = [1..28123]
