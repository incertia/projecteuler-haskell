module PE064 (solve064) where

import Common (cfrac', period)
import Math.NumberTheory.Powers (isSquare)

periodlength :: Integer -> Integer
periodlength n
  | isSquare n = 0
  | otherwise  = period (tail (cfrac' n))

solve064 :: String -> Integer
solve064 _ = toInteger . length . filter (odd . periodlength) $ [1..10000]
