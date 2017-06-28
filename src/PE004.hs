module PE004 (solve004) where

import Data.List (sort)

solve004 :: IO ()
solve004 = putStrLn ans
  where ans = last $ filter (\x -> x == reverse x) (show <$> prods)
        prods = sort $ (*) <$> nums <*> nums
        nums = [1..999]
