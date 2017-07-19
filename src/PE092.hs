module PE092 (solve092) where

import Data.Char (digitToInt)
import Data.List (group)

-- utilize the fact that the sum of squares is symmetric in the digits so we
-- only need to generate sets of 7 digits from the set [0..9]
-- 9^2 * 6 is small enough for end89 to run relatively quickly so we don't need
-- to memoize that
-- the factorials taken are also small enough
-- numer is always 7! but we compute it just to illustrate how you are supposed
-- to count in the generic case
-- special care needs to be given to `replicate 7 0` because 0 -> 0 is also a
-- chain but was not mentioned in the problem statement

combos :: [a] -> Int -> [[a]]
combos _ 0 = [[]]
combos [a] n = [replicate n a]
combos (a:as) n = [replicate k a ++ combo | k <- [0..n],
                                            combo <- combos as (n - k)]

end89 :: [Integer] -> Bool
end89 = end89' . sum . fmap (^2)
  where end89' 0  = False
        end89' 1  = False
        end89' 89 = True
        end89' n  = end89' . toInteger . sum . fmap ((^2) . digitToInt) . show
                  $ n

solve092 :: String -> Integer
solve092 _ = toInteger (sum counts)
  where
    fact n = product [1..n]
    counts = [cnt | combo <- combos [0..9] 7,
                    end89 combo,
                    let numer = fact (length combo),
                    let denom = product . fmap (fact . length) . group $ combo,
                    let cnt = numer `div` denom
             ]
