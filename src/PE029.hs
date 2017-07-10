module PE029 (solve029) where

import Control.Applicative ((<*>))
import Data.List (group, sort)

solve029 :: String -> Integer
solve029 _ = toInteger . length . group . sort $ fmap (^) [2..100] <*> [2..100]
