module Day01

import System.File
import Data.String
import Data.Maybe
import Data.SortedMap

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

distance : List Int -> Int
distance []            = 0
distance (x :: y :: _) = abs $ x - y
distance [_]           = 0

processInput : String -> Int
processInput content = sum . map distance . transpose . map sort . transpose
                     . map (map (readInt) . words) $ lines content

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

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let silver = show $ processInput content
                                     gold   = show $ processInput' content
                                     in putStrLn $ "Silver: " ++ silver
                                                ++ "\nGold: " ++ gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day01.txt"
