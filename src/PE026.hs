module PE026 (solve026) where

import Data.List (elemIndex, maximumBy)
import Data.Ord (comparing)

clen :: Integer -> Integer -> Integer
clen n d = clen' [] n d + 1

clen' :: [Integer] -> Integer -> Integer -> Integer
clen' rs n d = let r = n `mod` d in
  case r `elemIndex` rs of
       Just i  -> toInteger i
       Nothing -> clen' (r:rs) (10 * r) d

solve026 :: String -> Integer
solve026 _ = maximumBy (comparing (clen 1)) [1..999]
