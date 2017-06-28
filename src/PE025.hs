module PE025 (solve025) where

solve025 :: IO ()
solve025 = print . fst . head $ dropWhile cond (zip [1..] fibs)
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
        cond = (<1000) . length . show . snd
