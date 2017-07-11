module Solver (solve) where

import Control.Exception (try)
import qualified Data.HashMap.Lazy as H (HashMap, fromList, lookup)

import PE001 (solve001)
import PE002 (solve002)
import PE003 (solve003)
import PE004 (solve004)
import PE005 (solve005)
import PE006 (solve006)
import PE007 (solve007)
import PE008 (solve008)
import PE009 (solve009)
import PE010 (solve010)
import PE011 (solve011)
import PE012 (solve012)
import PE013 (solve013)
import PE014 (solve014)
import PE015 (solve015)
import PE016 (solve016)
import PE018 (solve018)
import PE020 (solve020)
import PE021 (solve021)
import PE022 (solve022)
import PE023 (solve023)
import PE024 (solve024)
import PE025 (solve025)
import PE029 (solve029)
import PE030 (solve030)
import PE031 (solve031)
import PE034 (solve034)
import PE035 (solve035)
import PE037 (solve037)
import PE048 (solve048)
import PE067 (solve067)

solvers :: H.HashMap Integer (String -> Integer)
solvers = H.fromList
  [ (001, solve001), (002, solve002), (003, solve003), (004, solve004), (005, solve005)
  , (006, solve006), (007, solve007), (008, solve008), (009, solve009), (010, solve010)
  , (011, solve011), (012, solve012), (013, solve013), (014, solve014), (015, solve015)
  , (016, solve016), (018, solve018), (020, solve020)
  , (021, solve021), (022, solve022), (023, solve023), (024, solve024), (025, solve025)
  , (029, solve029), (030, solve030)
  , (031, solve031), (034, solve034), (035, solve035)
  , (037, solve037)
  , (048, solve048)
  , (067, solve067)
  ]

solve :: Integer -> IO ()
solve n = case H.lookup n solvers of
  Nothing -> putStrLn "no solver implemented"
  Just f  -> do
    let ifile = "./input/" ++ pad 3 '0' (show n)
    print . f =<< readMaybeFile =<< try (readFile ifile)

  where pad :: Int-> b -> [b] -> [b]
        pad n x xs = if length xs < n then pad n x (x:xs) else xs

        readMaybeFile :: Either IOError String -> IO String
        readMaybeFile (Left _)  = return ""
        readMaybeFile (Right s) = return s
