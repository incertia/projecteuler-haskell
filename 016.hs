import Data.Char (digitToInt)

main :: IO ()
main = print . sum . fmap digitToInt . show $ 2^1000
