import Data.List (sort)

pf :: Integer -> [Integer]
pf 1 = []
pf n = p : pf (n `div` p)
  where p = head [x | x <- [2..n], n `mod` x == 0]

main :: IO ()
main = putStrLn $ show $ last . sort . pf $ 600851475143
