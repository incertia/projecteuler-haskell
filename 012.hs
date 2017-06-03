import Control.Monad (guard)
import Data.List (group)

pf :: Integer -> [Integer]
pf 1 = []
pf n = p : pf (n `div` p)
  where p = head [x | x <- [2..n], n `mod` x == 0]

nd :: Integer -> Integer
nd n = product $ (+1) . toInteger . length <$> (group . pf $ n)

trinums :: [Integer]
trinums = scanl1 (+) [1..]

main :: IO ()
main = print ans
  where ans = head [n | n <- trinums, nd n > 500]
