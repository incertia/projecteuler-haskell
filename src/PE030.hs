module PE030 (solve030) where

import Control.Arrow ((&&&), (>>>))
import Data.Char (digitToInt)

solve030 :: IO ()
solve030 = print . sum $ filter ((dsum &&& id) >>> uncurry (==)) [10..limit]
  where dsum = show >>> fmap (digitToInt >>> (^5)) >>> sum
        -- 999999 - (6 * 9^5) > 0
        -- limit = 999999
        -- 444444 - (6 * 9^5) > 0
        limit = 444444
