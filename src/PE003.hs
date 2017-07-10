module PE003 (solve003) where

pf :: Integral a => a -> [a]
pf 1 = []
pf n = p : pf (n `div` p)
  where p = head [x | x <- [2..n], n `mod` x == 0]

solve003 :: String -> Integer
solve003 _ = maximum . pf $ 600851475143
