module PE065 (solve065) where

import Data.Char (digitToInt)
import Data.Ratio (Ratio, numerator, (%))

conv :: Integer -> [Integer] -> Ratio Integer
conv 1 (x:_) = x % 1
conv n (x:xs) = (x % 1) + recip (conv (n - 1) xs)

solve065 :: String -> Integer
solve065 _ = sum . fmap (toInteger . digitToInt) . show . numerator . conv 100
           $ e
  where e = 2 : concat [[1, 2 * k, 1] | k <- [1..]]
