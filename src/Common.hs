module Common where

import Data.Bits (shiftR)
import Data.Ratio (Ratio, numerator, (%))

-- (iroot n)^2 = iroot n^2 is confusing so give it a name
hasiroot :: Integer -> Bool
hasiroot n = r^2 == n where r = iroot n

iroot :: Integer -> Integer
iroot n = case n `compare` 0 of
  LT -> error "wtf"
  EQ -> 0
  GT -> iroot' one 0 n
  where one = maximum . takeWhile (<=n) $ (4^) <$> [0..]
        iroot' :: Integer -> Integer -> Integer -> Integer
        iroot' 0 res _ = res
        iroot' one res op = iroot' one' res'' op'
          where (op', res') = if op >= res + one
                                 then (op - res - one, res + 2 * one)
                                 else (op, res)
                one'  = one `shiftR` 2
                res'' = res' `shiftR` 1

conv :: Integer -> [Integer] -> Ratio Integer
conv 1 (x:_) = x % 1
conv n (x:xs) = (x % 1) + recip (conv (n - 1) xs)
