module PE067 (solve067) where

-- this is the exact same as 018.hs

solve067 :: String -> Integer
solve067 = head . foldr1 dp . nums
  where nums = fmap (fmap read . words) . lines
        dp xs ys = zipWith3 dp' xs ys $ tail ys
        dp' x y z = x + max y z
