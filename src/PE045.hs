module PE045 (solve045) where

import Common (hasiroot, iroot)

-- notice that hexagonal numbers are also triangular numbers
-- n * (2n - 1) = (2n - 1) * (2n) / 2
-- we just need to solve a quadratic equation for pentagonal numbers and check
-- if the solution is an integer
-- 3n^2 - n - 2k = 0

solve045 :: String -> Integer
solve045 _ = head . dropWhile (<=40755) . filter ispent $ hex
  where hex = [n * (2 * n - 1) | n <- [1..]]
        ispent = and . sequence [hasiroot, (==0) . (`mod` 6) . succ . iroot]
                     . succ . (24*)
