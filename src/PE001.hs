module PE001 (solve001) where

solve001 :: IO ()
solve001 = print $ sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
