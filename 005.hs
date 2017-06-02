main :: IO ()
main = putStrLn $ show $ foldr lcm 1 [1..20]
