import Data.Numbers.Primes (primes)

main :: IO ()
main = putStrLn $ show $ sum $ takeWhile (<2000000) primes
