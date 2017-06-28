module PE018 (solve018) where

solve018 :: IO ()
solve018 = do
  nums <- fmap (fmap read . words) . lines <$> readFile "./input/018"
  (print . head) $ foldr1 dp nums
        -- leverage the fact that zip will truncate lists to the shorter list
        -- ys should be one longer than xs, but zip will truncate to xs
  where dp xs ys = zipWith3 dp' xs ys $ tail ys
        dp' x y z = x + max y z
