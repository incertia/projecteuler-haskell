module PE024 (solve024) where

import Data.List (sort)

perms :: Integer -> [a] -> [a]
perms n [] = []
perms n xs = if n >= fl then perms (n `mod` fl) xs else perm'
  where l = toInteger $ length xs
        fl = fact l
        fl' = fact (l - 1)
        fact n = product [1..n]
        perm' = elem : perms (n `mod` fl') (take n' xs ++ drop (n' + 1) xs)
        elem = xs !! n'
        n' = fromInteger $ n `div` fl'

solve024 :: String -> Integer
solve024 _ = read . perms 999999 $ ['0'..'9']
