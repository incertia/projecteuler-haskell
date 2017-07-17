module PE039 (solve039) where

import Data.List (maximumBy, group, sort, nub)
import Data.Ord (comparing)

solve039 :: String -> Integer
solve039 _ = head . maximumBy (comparing length) . group . sort $ perims

-- there should be a better way to enumerate all primitive triples
-- i'm too lazy and this is fast enough for 1000
perims = (\(a, b, c) -> a + b + c) <$> nub triples
triples = [(a' * g', b' * g', c' * g') | m <- [1..23],
                                         n <- [1..m],
                                         m > n,
                                         let a = m^2 - n^2,
                                         let b = 2 * m * n,
                                         let c = m^2 + n^2,
                                         let g = foldr gcd 0 [a, b, c],
                                         let a' = a `div` g,
                                         let b' = b `div` g,
                                         let c' = c `div` g,
                                         g' <- [1..1000 `div` (a' + b' + c')]]
