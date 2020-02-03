module PE097 (solve097) where

import Math.NumberTheory.Moduli (modulo, powSomeMod, getVal, SomeMod(..))

solve097 :: String -> Integer
solve097 _ = case 28433 * powSomeMod (2 `modulo` 10^10) 7830457 + 1 of
                  SomeMod k -> getVal k
                  _         -> error "impossible"
