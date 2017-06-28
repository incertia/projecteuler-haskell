module PE034 (solve034) where

import Control.Arrow ((&&&), (>>>))
import Data.Char (digitToInt)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

solve034 :: IO ()
solve034 = print . sum $ filter cond [10..999999]
  where cond = (fsum &&& id) >>> uncurry (==)
        fsum = show >>> fmap (digitToInt >>> toInteger >>> fact) >>> sum
