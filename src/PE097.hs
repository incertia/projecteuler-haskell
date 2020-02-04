{-# LANGUAGE DataKinds #-}

module PE097 (solve097) where

import Math.NumberTheory.Moduli (Mod, powMod, getVal)

solve097 :: String -> Integer
solve097 _ = getVal $ 28433 * powMod (2 :: Mod 10000000000) 7830457 + 1
