module PE062 (solve062) where

import Control.Arrow ((&&&))
import Data.List (sortBy, groupBy, minimumBy, sort)
import Data.Ord (comparing)

-- 10000 is just a bound, not sure how to make this work on an infinite list
solve062 :: String -> Integer
solve062 _ = minimum . fmap (snd . minimumBy (comparing snd))
               . filter ((==5) . length) . groupBy (((==EQ) .) . comparing fst)
               . sortBy (comparing fst) . fmap (sort . show &&& id) $ cubes
  where cubes = (^3) <$> [1..10000]
