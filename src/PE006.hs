module PE006 (solve006) where

solve006 :: String -> Integer
solve006 _ = abs $ ss - sq
  where ss = sum $ (^2) <$> nums
        sq = (^2) $ sum nums
        nums = [1..100]
