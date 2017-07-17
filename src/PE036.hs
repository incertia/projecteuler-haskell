{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PE036 (solve036) where

-- this problem is easily doable with Numeric.showIntAtBase but i want to
-- demonstrate the power of defining a Num instance

newtype Base2 = Base2 Integer
  deriving (Num, Ord, Eq)

instance Show Base2
  where show (Base2 n) = concatMap show (rems n)
          where rems 0 = []
                rems n = rems (n `div` 2) ++ [n `mod` 2]

newtype Base10 = Base10 Integer
  deriving (Num, Ord, Eq)

instance Show Base10
  where show (Base10 n) = show n

ispal :: Show a => a -> Bool
ispal x = x' == reverse x' where x' = show x

solve036 :: String -> Integer
solve036 _ = sum [x | x <- [1..999999],
                      -- filter with base10 first, because it kills more
                      ispal (fromInteger x :: Base10),
                      ispal (fromInteger x :: Base2)]
