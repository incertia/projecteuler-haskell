main :: IO ()
main = do
  nums <- fmap read . lines <$> readFile "./input/013"
  putStrLn $ take 10 . show $ sum nums
