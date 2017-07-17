module Common where

import Data.Bits (shiftR)

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
