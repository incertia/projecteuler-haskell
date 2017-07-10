module PE016 (solve016) where

import Data.Char (digitToInt)

solve016 :: String -> Integer
solve016 _ = toInteger . sum . fmap digitToInt . show $ 2^1000
