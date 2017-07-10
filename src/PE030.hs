module PE030 (solve030) where

import Control.Arrow ((&&&), (>>>))
import Data.Char (digitToInt)

solve030 :: String -> Integer
solve030 _ = sum . filter ((dsum &&& id) >>> uncurry (==)) $ [10..limit]
  where dsum = sum . fmap ((^5 ) . toInteger . digitToInt) . show
        -- 999999 - (6 * 9^5) > 0
        -- limit = 999999
        -- 444444 - (6 * 9^5) > 0
        limit = 444444
