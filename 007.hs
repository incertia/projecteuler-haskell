-- wheel sieve is much faster
import Data.Numbers.Primes (primes)

-- primes :: [Integer]
-- primes = sieve [2..]
--   where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = putStrLn $ show $ primes !! 10000
