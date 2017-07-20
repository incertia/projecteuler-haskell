module PE065 (solve065) where

import Common (conv)
import Data.Char (digitToInt)
import Data.Ratio (numerator)

solve065 :: String -> Integer
solve065 _ = sum . fmap (toInteger . digitToInt) . show . numerator . conv 100
           $ e
  where e = 2 : concat [[1, 2 * k, 1] | k <- [1..]]
