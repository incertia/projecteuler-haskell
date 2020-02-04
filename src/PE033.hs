module PE033 (solve033) where

import Data.Ratio (Ratio(..), (%), denominator)

bad :: Integer -> Integer -> Bool
bad x y
  | q >= 1 = False
  | x0 == 0 || y0 == 0 = False
  | x0 /= y1 = False
  | otherwise = q == x1 % y0
  where (x1, x0) = x `divMod` 10
        (y1, y0) = y `divMod` 10
        q = x % y

solve033 :: String -> Integer
solve033 _ = denominator . product . fmap (uncurry (%)) . filter (uncurry bad) $
  (,) <$> [11..99] <*> [11..99]
