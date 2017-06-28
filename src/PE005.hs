module PE005 (solve005) where

solve005 :: IO ()
solve005 = putStrLn $ show $ foldr lcm 1 [1..20]
