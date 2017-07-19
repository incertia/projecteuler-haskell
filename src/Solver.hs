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
import PE027 (solve027)
import PE028 (solve028)
import PE029 (solve029)
import PE030 (solve030)
import PE032 (solve032)
import PE034 (solve034)
import PE035 (solve035)
import PE036 (solve036)
import PE037 (solve037)
import PE038 (solve038)
import PE039 (solve039)
import PE040 (solve040)
import PE041 (solve041)
import PE042 (solve042)
import PE045 (solve045)
import PE046 (solve046)
import PE047 (solve047)
import PE048 (solve048)
import PE062 (solve062)
import PE065 (solve065)
import PE067 (solve067)
import PE072 (solve072)
import PE079 (solve079)
import PE092 (solve092)

solvers :: H.HashMap Integer (String -> Integer)
solvers = H.fromList
  [ (001, solve001), (002, solve002), (003, solve003), (004, solve004), (005, solve005)
  , (006, solve006), (007, solve007), (008, solve008), (009, solve009), (010, solve010)
  , (011, solve011), (012, solve012), (013, solve013), (014, solve014), (015, solve015)
  , (016, solve016), (018, solve018), (020, solve020)
  , (021, solve021), (022, solve022), (023, solve023), (024, solve024), (025, solve025)
  , (027, solve027), (028, solve028), (029, solve029), (030, solve030)
  , (032, solve032), (034, solve034), (035, solve035)
  , (036, solve036), (037, solve037), (038, solve038), (039, solve039), (040, solve040)
  , (041, solve041), (042, solve042), (045, solve045)
  , (046, solve046), (047, solve047), (048, solve048)
  , (062, solve062), (065, solve065)
  , (067, solve067)
  , (072, solve072)
  , (079, solve079)
  , (092, solve092)
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
