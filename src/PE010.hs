module PE010 (solve010) where

import Data.Numbers.Primes (primes)

solve010 :: IO ()
solve010 = putStrLn $ show $ sum $ takeWhile (<2000000) primes
