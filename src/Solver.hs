module Solver (solve) where

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
import PE025 (solve025)
import PE029 (solve029)
import PE030 (solve030)
import PE048 (solve048)
import PE067 (solve067)

solve :: Integer -> IO ()
solve   1 = solve001
solve   2 = solve002
solve   3 = solve003
solve   4 = solve004
solve   5 = solve005
solve   6 = solve006
solve   7 = solve007
solve   8 = solve008
solve   9 = solve009
solve  10 = solve010
solve  12 = solve012
solve  13 = solve013
solve  14 = solve014
solve  15 = solve015
solve  16 = solve016
solve  18 = solve018
solve  20 = solve020
solve  21 = solve021
solve  22 = solve022
solve  23 = solve023
solve  25 = solve025
solve  29 = solve029
solve  30 = solve030
solve  48 = solve048
solve  67 = solve067
solve   _ = putStrLn "no solver implemented"
