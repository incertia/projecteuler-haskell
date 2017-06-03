import Data.Char (digitToInt)

main :: IO ()
main = print . sum . fmap digitToInt . show . product $ [1..100]
