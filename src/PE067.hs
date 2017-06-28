module PE067 (solve067) where

-- this is the exact same as 018.hs

solve067 :: IO ()
solve067 = do
  nums <- fmap (fmap read . words) . lines <$> readFile "./input/067"
  (print . head) $ foldr1 dp nums
  where dp xs ys = zipWith3 dp' xs ys $ tail ys
        dp' x y z = x + max y z
