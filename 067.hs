-- this is the exact same as 018.hs

main :: IO ()
main = do
  nums <- fmap (fmap words) . fmap words . lines <$> readFile "./input/067"
  print $ head $ foldr1 dp nums
  where dp xs ys = zipWith3 dp' xs ys $ tail ys
        dp' x y z = x + max y z
