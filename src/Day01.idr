module Day01

import System.File
import Data.String
import Data.Maybe
import Data.SortedMap

readInt : String -> Int
readInt s = fromMaybe 0 $ parseInteger {a=Int} s

distance : List Int -> Int
distance []             = 0
distance (x :: y :: xs) = abs $ x - y
distance [_]            = 0

processInput : String -> Int
processInput content = sum . map distance . transpose . map sort . transpose
                     . map (map (readInt) . words) $ lines content

solve1 : IO ()
solve1 = do file <- readFile path
            case file of
                 Right content => putStrLn . ("A: " ++) . show
                                $ processInput content
                 Left  error   => putStrLn (show error)
  where
    path = "./rsc/day01.txt"

numOccurance : List Int -> Int -> Int
numOccurance xs y = cast . length . takeWhile (== y) . dropWhile (< y) $ xs

sim : (SortedMap Int Int) -> List Int -> List Int -> List (Int, Int)
sim dict bs (a :: as) = (a, score) :: (sim (insert a score dict) bs as)
  where
    score = fromMaybe (numOccurance bs a) $ lookup a dict
sim _    _  []        = []

compSim : List (List Int) -> List (Int, Int)
compSim (as :: bs :: _) = sim empty (sort bs) as
compSim []              = []
compSim [_]             = []

processInput' : String -> Int
processInput' content = sum . map (\(a,s) => a * s) . compSim . transpose
                      . map (map (readInt) . words) $ lines content

solve2 : IO ()
solve2 = do file <- readFile path
            case file of
                 Right content => putStrLn . ("B: " ++) . show
                                $ processInput' content
                 Left  error   => putStrLn (show error)
  where
    path = "./rsc/day01.txt"

public export
solve : IO ()
solve = solve1 >> solve2
