module PE003 (solve003) where

pf :: Integer -> [Integer]
pf 1 = []
pf n = p : pf (n `div` p)
  where p = head [x | x <- [2..n], n `mod` x == 0]

solve003 :: IO ()
solve003 = print $ maximum . pf $ 600851475143
