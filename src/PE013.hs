module PE013 (solve013) where

solve013 :: String -> Integer
solve013 = read . take 10 . show . sum . fmap read . lines
