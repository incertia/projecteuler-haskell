module PE013 (solve013) where

solve013 :: IO ()
solve013 = do
  nums <- fmap read . lines <$> readFile "./input/013"
  putStrLn $ take 10 . show $ sum nums
