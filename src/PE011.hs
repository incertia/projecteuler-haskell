module PE011 (solve011) where

import Control.Arrow (first, second, (***))
import Data.Array (Array, Ix, listArray, (!), bounds, indices, inRange)

solve011 :: IO ()
solve011 = do
  input <- readFile "./input/011"
  let a = listArray ((1, 1), (20, 20)) $ fmap read . words $ input
  let prods = [product xs | i <- indices a,
                            d <- directions,
                            let indices = take 4 $ iterate d i,
                            all (inbounds a) indices,
                            let xs = (a!) <$> indices
              ]
  print . maximum $ prods

  where inbounds :: Ix i => Array i e -> i -> Bool
        inbounds = inRange . bounds

        -- superfluous directions (we technically only need half)
        directions :: Num a => [(a, a) -> (a, a)]
        directions = [first inc, second inc, first dec, second dec,
                      inc *** inc, dec *** inc, inc *** dec, dec *** dec]
          where inc = (+1)
                dec = (+(-1))
