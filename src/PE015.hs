module PE015 (solve015) where

-- use combinatorics to avoid brute force :)

solve015 :: IO ()
solve015 = print $ binom (w + h) w
  where binom n k = fact n `div` ((fact k) * (fact (n - k)))
        fact n = product [1..n]
        w = 20
        h = 20
