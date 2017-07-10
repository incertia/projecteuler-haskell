module PE004 (solve004) where

import Data.List (sort)

solve004 :: String -> Integer
solve004 _ = fromIntegral . maximum . filter (\x -> let x' = show x in x' == reverse x') $ prods
  where prods = (*) <$> nums <*> nums
        nums = [1..999]
