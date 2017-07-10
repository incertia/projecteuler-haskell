module PE018 (solve018) where

solve018 :: String -> Integer
solve018 = head . foldr1 dp . nums
  where nums = fmap (fmap read . words) . lines
        -- leverage the fact that zip will truncate lists to the shorter list
        -- ys should be one longer than xs, but zip will truncate to xs
        dp xs ys = zipWith3 dp' xs ys $ tail ys
        dp' x y z = x + max y z
