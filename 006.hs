main :: IO ()
main = putStrLn $ show ans
  where ans = abs $ ss - sq
        ss = sum $ zipWith (*) nums nums
        sq = let s = sum nums in s * s
        nums = [1..100]
