module PE042 (solve042) where

import Common (hasiroot, iroot)
import Data.Char (ord)

triword :: String -> Bool
triword = and . sequence [hasiroot, odd . iroot] . succ . (*8) . sum
        . fmap (toInteger . (\x -> x - ord 'A' + 1) . ord)

solve042 :: String -> Integer
solve042 input = toInteger . length . filter triword . read $ "[" ++ input ++ "]"
