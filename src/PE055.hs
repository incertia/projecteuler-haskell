module PE055 (solve055) where

lychrel :: Integer -> Bool
lychrel = flip lychrel' 0
  where lychrel' :: Integer -> Integer -> Bool
        lychrel' n k
          | k >= 50 = True
          | otherwise = not (palindrome (show n')) && lychrel' n' (k + 1)
          where n' = n + read (reverse (show n))
        palindrome x = x == reverse x

solve055 :: String -> Integer
solve055 _ = toInteger . length . filter lychrel $ [1..9999]
