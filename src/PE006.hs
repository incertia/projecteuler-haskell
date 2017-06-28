module PE006 (solve006) where

solve006 :: IO ()
solve006 = putStrLn $ show ans
  where ans = abs $ ss - sq
        ss = sum $ (^2) <$> nums
        sq = (^2) $ sum nums
        nums = [1..100]
