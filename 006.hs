main :: IO ()
main = putStrLn $ show ans
  where ans = abs $ ss - sq
        ss = sum $ (^2) <$> nums
        sq = (^2) $ sum nums
        nums = [1..100]
