module PE066 (solve066) where

import Common (conv)
import Control.Arrow ((&&&))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Ratio (numerator, denominator)
import Math.NumberTheory.Powers (integerSquareRoot, isSquare)

-- this is just pell's equation x^2 - Dy^2 = 1
-- minimal x can be found by substituting x/y from convergents of sqrt D

cfrac :: Integer -> [Integer]
cfrac n
  | isSquare n = [a0]
  | otherwise  = [a_i | (_, _, a_i) <- stuff]
  where stuff = iterate next (0, 1, a0)
        a0 = integerSquareRoot n
        next (m, d, a) = (m', d', a')
          where m' = d * a - m
                d' = (n - m'^2) `div` d
                a' = (a0 + m') `div` d'

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
