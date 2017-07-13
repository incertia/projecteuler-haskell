module PE079 (solve079) where

import Data.Char (digitToInt)
import Data.Graph (buildG, topSort)
import Data.List (intersect)

-- let's just assume that each digit appears only once, so we can do a
-- topological sort to read everything in a correct order

solve079 :: String -> Integer
solve079 f = read . (`intersect` used) . concatMap show . topSort
               . buildG (0, 9) $ edges
  where edges = concatMap edgelist . fmap (fmap digitToInt) . lines $ f
        used = ['0'..'9'] `intersect` f
        edgelist [a, b, c] = [(a, b), (b, c)]
