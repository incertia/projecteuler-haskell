module PE020 (solve020) where

import Data.Char (digitToInt)

solve020 :: IO ()
solve020 = print . sum . fmap digitToInt . show . product $ [1..100]
