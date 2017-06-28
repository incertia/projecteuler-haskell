module PE016 (solve016) where

import Data.Char (digitToInt)

solve016 :: IO ()
solve016 = print . sum . fmap digitToInt . show $ 2^1000
