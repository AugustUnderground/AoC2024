module Day02

import System.File
import Data.String
import Data.Maybe
import Data.Nat
import Data.SortedMap

data Puzzle = Silver | Gold

Show Puzzle where
  show Silver = "Silver"
  show Gold   = "Gold"

Eq Puzzle where
  Silver == Silver = True
  Gold   == Gold   = True
  _      == _      = False

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

dif : List Int -> Bool
dif (a :: b :: xs) = if (a < b) && ((b - a) <= 3) then dif (b :: xs) else False
dif []             = True
dif [_]            = True

processReport : List Int -> Int
processReport report = if ((isInc || isDec) && isDif) then 1 else 0
  where
    isInc = (report ==) $ sort report
    isDec = (report ==) . reverse $ sort report
    isDif = dif $ sort report

deleteAt' : Nat -> List a -> List a
deleteAt' idx xs = i ++ t
  where
    i = take idx xs
    t = drop (idx + 1) xs

dampen : Nat -> List Int -> Int
dampen idx report with (processReport $ deleteAt' idx report)
  dampen idx report | 1 = 1
  dampen idx report | _ with ((idx + 1) < (length report))
    dampen idx report | _ | True  = dampen (idx + 1) report
    dampen idx report | _ | False = 0

processReport' : List Int -> Int
processReport' report with (processReport report)
  processReport' report | 1 = 1
  processReport' report | _ = dampen 0 report

processInput : Puzzle -> String -> Int
processInput puzzle content = sum $ map (proc) reports
  where
    proc    = if puzzle == Silver then processReport else processReport'
    reports = map (map readInt . words) $ lines content

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let silver = show $ processInput Silver content
                                     gold   = show $ processInput Gold   content
                                     in putStrLn $ "Silver: " ++ silver
                                                ++ "\nGold: " ++ gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day02.txt"
