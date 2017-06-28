module Main where

import Solver (solve)
import System.Environment (getProgName, getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  pname <- getProgName
  args <- getArgs

  -- really not sure of the best way to bind `pname` to a function
  let usage :: IO ()
      usage = putStrLn $ "usage: " ++ pname ++ " <problem-number>"

  case args of
    [n] -> case readMaybe n :: Maybe Integer of
             Just n' -> solve n'
             Nothing -> usage
    _   -> usage
