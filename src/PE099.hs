module PE099 (solve099) where

import Data.List (elemIndex, maximumBy)
import Data.Maybe (fromJust)

solve099 :: String -> Integer
solve099 input = succ . toInteger . fromJust . flip elemIndex exps . maximumBy f
               $ exps
  where exps = [read ("(" ++ l ++ ")") | l <- lines input]
        (a, b) `f` (c, d) = b' `compare'` (d' * logBase a' c')
                -- coerce double for maximum precision
          where compare' :: Double -> Double -> Ordering
                compare' = compare
                [a', b', c', d'] = fromInteger <$> [a, b, c, d]
