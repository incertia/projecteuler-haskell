module PE056 (solve056) where

import Data.Char (digitToInt)

solve056 :: String -> Integer
solve056 _ = maximum . fmap (sum . fmap (toInteger . digitToInt) . show)
           $ (^) <$> [1..99] <*> [1..99]
