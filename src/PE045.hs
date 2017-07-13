module PE045 (solve045) where

import Data.Bits (shiftR)
import Data.List (intersect)

-- notice that hexagonal numbers are also triangular numbers
-- n * (2n - 1) = (2n - 1) * (2n) / 2
-- we just need to solve a quadratic equation for pentagonal numbers and check
-- if the solution is an integer
-- 3n^2 - n - 2k = 0

-- (iroot n)^2 = iroot n^2 is confusing so give it a name
hasiroot :: Integer -> Bool
hasiroot n = r^2 == n where r = iroot n

iroot :: Integer -> Integer
iroot n = iroot' one 0 n
  where one = maximum . takeWhile (<=n) $ (4^) <$> [1..]
        iroot' :: Integer -> Integer -> Integer -> Integer
        iroot' 0 res _ = res
        iroot' one res op = iroot' one' res'' op'
          where (op', res') = if op >= res + one
                                 then (op - res - one, res + 2 * one)
                                 else (op, res)
                one'  = one `shiftR` 2
                res'' = res' `shiftR` 1

solve045 :: String -> Integer
solve045 _ = head . dropWhile (<=40755) . filter ispent $ hex
  where hex = [n * (2 * n - 1) | n <- [1..]]
        ispent = and . sequence [hasiroot, (==0) . (`mod` 6) . succ . iroot]
                     . succ . (24*)
