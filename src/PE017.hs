module PE017 (solve017) where

lc :: Integer -> String
lc n
  | n == 1000 = "onethousand"
  | n >= 100  = lc q ++ "hundred" ++ if r == 0 then "" else "and" ++ lc r
  | n >= 90   = "ninety" ++ lc (n `mod` 10)
  | n >= 80   = "eighty" ++ lc (n `mod` 10)
  | n >= 70   = "seventy" ++ lc (n `mod` 10)
  | n >= 60   = "sixty" ++ lc (n `mod` 10)
  | n >= 50   = "fifty" ++ lc (n `mod` 10)
  | n >= 40   = "forty" ++ lc (n `mod` 10)
  | n >= 30   = "thirty" ++ lc (n `mod` 10)
  | n >= 20   = "twenty" ++ lc (n `mod` 10)
  | n == 19   = "nineteen"
  | n == 18   = "eighteen"
  | n == 17   = "seventeen"
  | n == 16   = "sixteen"
  | n == 15   = "fifteen"
  | n == 14   = "fourteen"
  | n == 13   = "thirteen"
  | n == 12   = "twelve"
  | n == 11   = "eleven"
  | n == 10   = "ten"
  | n == 9    = "nine"
  | n == 8    = "eight"
  | n == 7    = "seven"
  | n == 6    = "six"
  | n == 5    = "five"
  | n == 4    = "four"
  | n == 3    = "three"
  | n == 2    = "two"
  | n == 1    = "one"
  | n == 0    = ""
  where (q, r) = n `divMod` 100

solve017 :: String -> Integer
solve017 _ = toInteger . length . concatMap lc $ [1..1000]
