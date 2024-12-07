module Day02

import System.File
import Data.String
import Data.Maybe

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

dif : List Int -> Bool
dif (a :: b :: xs) with ((a < b) && ((b - a) <= 3))
  dif (_ :: b :: xs) | True  = dif (b :: xs) 
  _                  | _     = False
dif []                       = True
dif [_]                      = True

processReport : List Int -> Int
processReport report = if ((isInc || isDec) && isDif) then 1 else 0
  where
    isInc = (report ==) $ sort report
    isDec = (report ==) . reverse $ sort report
    isDif = dif $ sort report

deleteAt' : Nat -> List a -> List a
deleteAt' idx xs with (idx + 1)
  _ | j = (take idx xs) ++ (drop j xs)

dampen : Nat -> List Int -> Int
dampen idx report with (processReport $ deleteAt' idx report) 
                     | ((idx + 1) < (length report))
  dampen idx report | 1 | _     = 1
  dampen idx report | _ | True  = dampen (idx + 1) report
  dampen idx report | _ | False = 0

processReport' : List Int -> Int
processReport' report with (processReport report)
  processReport' report | 1 = 1
  processReport' report | _ = dampen 0 report

processInput : Nat -> List (List Int) -> Int
processInput 0 reports = sum $ map processReport  reports
processInput _ reports = sum $ map processReport' reports

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let reports = map (map readInt . words) $ lines content
                                     silver  = show $ processInput 0 reports
                                     gold    = show $ processInput 1 reports
                                     in putStrLn $ "\tSilver: " ++ silver
                                              ++ "\n\tGold:   " ++ gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day02.txt"
