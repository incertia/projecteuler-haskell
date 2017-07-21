module PE066 (solve066) where

import Common (cfrac, conv)
import Control.Arrow ((&&&))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Ratio (numerator, denominator)
import Math.NumberTheory.Powers (isSquare)

-- this is just pell's equation x^2 - Dy^2 = 1
-- minimal x can be found by substituting x/y from convergents of sqrt D

fundamental :: Integer -> (Integer, Integer)
fundamental d = head [(x, y) | (x, y) <- split <$> sequence convergents cf',
                                x^2 - d * y^2 == 1]
  where split = numerator &&& denominator
        convergents = conv <$> [1..]
        cf' = cfrac d

solve066 :: String -> Integer
solve066 _ = fst . maximumBy (comparing snd) . fmap f $ ds
  where ds = filter (not . isSquare) [1..1000]
        f d = (d, fst (fundamental d))
