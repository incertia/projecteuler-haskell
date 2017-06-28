module PE014 (solve014) where

import Data.MemoCombinators (integral)
import Data.List (maximumBy)
import Data.Ord (comparing)

cl :: Integer -> Integer
cl = integral cl'
  where cl' :: Integer -> Integer
        cl' 1 = 1
        cl' n = 1 + if even n then cl (n `div` 2) else cl (3 * n + 1)

solve014 :: IO ()
solve014 = print . fst . maximumBy (comparing snd) $ zip nums $ cl <$> nums
  where nums = [1..1000000]
