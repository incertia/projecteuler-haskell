import Data.List (sort)

main :: IO ()
main = putStrLn ans
  where ans = last $ filter (\x -> x == reverse x) (show <$> prods)
        prods = sort $ (*) <$> nums <*> nums
        nums = [1..999]
