module PE097 (solve097) where

import Math.NumberTheory.Moduli (powerModInteger)

solve097 :: String -> Integer
solve097 _ = (28433 * powerModInteger 2 7830457 (10^10) + 1) `mod` (10^10)
