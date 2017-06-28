module PE048 (solve048) where

import Control.Arrow ((&&&), (>>>))

solve048 :: IO ()
solve048 = print $ sum (((id &&& id) >>> uncurry (^)) <$> [1..1000]) `mod` 10^10
