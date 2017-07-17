module Main (main) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Solver (solve)
import System.Environment (getProgName, getArgs)
import Text.Read (readMaybe)

timed :: IO () -> IO ()
timed io = do
  start <- getPOSIXTime
  io
  end <- getPOSIXTime
  putStrLn $ "solution took " ++ show (round ((end - start) * 1000)) ++ "ms"

main :: IO ()
main = do
  pname <- getProgName
  args <- getArgs

  -- really not sure of the best way to bind `pname` to a function
  let usage :: IO ()
      usage = putStrLn $ "usage: " ++ pname ++ " <problem-number>"

  case args of
    [n] -> case readMaybe n :: Maybe Integer of
             Just n' -> timed $ solve n'
             Nothing -> usage
    _   -> usage
