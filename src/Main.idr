module Main

import Data.List

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day12

solutions : List (String, IO ())
solutions = zip days sols
  where
    days = map show [1 .. 7]
    sols = [ Day01.solve, Day02.solve, Day03.solve, Day04.solve, Day05.solve
           , Day06.solve, Day07.solve ]

run : List (String, IO ()) -> IO ()
run [] = pure ()
run ((day,sol) :: days) = do
  putStrLn $ "Day " ++ day ++ ":"
  sol
  run days

main : IO ()
main = Day12.solve
  -- run solutions
