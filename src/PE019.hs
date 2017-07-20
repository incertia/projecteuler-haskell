module PE019 (solve019) where

nonleap :: [Integer]
nonleap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

leap :: [Integer]
leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

months :: Integer -> [Integer]
months y
  | y `mod` 400 == 0 = leap
  | y `mod` 100 == 0 = nonleap
  | y `mod` 4   == 0 = leap
  | otherwise        = nonleap

solve019 :: String -> Integer
solve019 _ = toInteger . length . filter ((==0) . (`mod` 7)) . scanl (+) 0
           . concatMap months $ [1901..2000]
