module PE009 (solve009) where

import Control.Monad (guard)

solve009 :: String -> Integer
solve009 _ = head $ do
  a <- [1..998]
  b <- [1..999 - a]
  let c = 1000 - a - b
  guard $ a^2 + b^2 == c^2
  return $ a * b * c
