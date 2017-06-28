module PE029 (solve029) where

import Control.Applicative ((<*>))
import Data.List (group, sort)

solve029 :: IO ()
solve029 = print $ length . group . sort $ fmap (^) [2..100] <*> [2..100]
