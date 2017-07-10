module PE020 (solve020) where

import Data.Char (digitToInt)

solve020 :: String -> Integer
solve020 _ = toInteger . sum . fmap digitToInt . show . product $ [1..100]
