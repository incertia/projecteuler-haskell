module PE022 (solve022) where

import Data.Char (ord)
import Data.List (sort)

solve022 :: IO ()
solve022 = do
  inp <- readFile "./input/022"
  let names = sort . read $ "[" ++ inp ++ "]"
      scores = zipWith (*) (score <$> names) [1..]

  print $ sum scores
  where score :: String -> Integer
        score = foldr (\c s -> toInteger (ord c - ord 'A') + 1 + s) 0
