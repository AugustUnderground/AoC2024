module Main

import Data.List
import Data.String

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
import Day11
import Day12
import Day13
import Day14
import Day15
import Day17
import Day16
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24

solutions : List (IO ())
solutions = [ Day01.solve, Day02.solve, Day03.solve, Day04.solve, Day05.solve
            , Day06.solve, Day07.solve, Day08.solve, Day09.solve, Day10.solve
            , Day11.solve, Day12.solve, Day13.solve, Day14.solve, Day15.solve
            , Day16.solve, Day17.solve, Day18.solve, Day19.solve, Day20.solve
            , Day21.solve, Day22.solve, Day23.solve, Day24.solve -- , Day25.solve
            ]

run : List (IO ()) -> IO ()
run      []       = putStrLn $ "✧･ﾟ: *✧･ﾟ:* " ++ len
                            ++ " solutions for AoC 2024" ++ " *:･ﾟ✧*:･ﾟ✧ "
  where
    len = padLeft 2 '0' . show . cast {to=Int} $ length solutions
run (sol :: sols) = putStrLn day >> sol >> run sols
  where
    len = cast {to=Int} $ length solutions
    day = (++":") . ("Day " ++) . padLeft 2 '0' . show . (len -)
        . cast {to=Int} $ length sols

main : IO ()
main = do
  Day24.solve
  -- run solutions
