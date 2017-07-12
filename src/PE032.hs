module PE032 (solve032) where

import Control.Arrow ((&&&), (>>>))
import Data.List (group, sort)

pandigital :: Integer -> Bool
pandigital = or . fmap cond . prods
  where prods n = [[a, n `div` a, n] | a <- [1..n], n `mod` a == 0]
        cond :: [Integer] -> Bool
        cond = (=="123456789") . sort . concatMap show

solve032 :: String -> Integer
solve032 _ = sum . filter pandigital $ [1234..9876]
