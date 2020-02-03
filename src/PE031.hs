module PE031 (solve031) where

ways :: [Integer] -> Integer -> Integer
ways [] 0 = 1
ways [] _ = 0
ways xx@(x:xs) n = case n `compare` 0 of
                        LT -> 0
                        EQ -> 1
                        GT -> ways xx (n - x) + ways xs n

solve031 :: String -> Integer
solve031 _ = ways [1, 2, 5, 10, 20, 50, 100, 200] 200
