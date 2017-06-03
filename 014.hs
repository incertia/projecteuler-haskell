import Data.Function.Memoize (memoFix)
import Data.List (maximumBy)
import Data.Ord (comparing)

cl :: Integer -> Integer
cl = memoFix cl'
  where cl' :: (Integer -> Integer) -> Integer -> Integer
        cl' _ 1 = 1
        cl' f n = 1 + if even n then f (n `div` 2) else f (3 * n + 1)

main :: IO ()
main = print . fst . maximumBy (comparing snd) $ zip nums $ cl <$> nums
  where nums = [1..1000000]
