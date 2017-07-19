module PE052 (solve052) where

import Data.List (group, sort)

-- this is really asking if you know the decimal representation of 1/7 well
-- enough

solve052 :: String -> Integer
solve052 _ = head . head . filter ((==1) . length . group . fmap (sort . show))
           . fmap (sequence ((*) <$> [1..6])) $ [1..]
