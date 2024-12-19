module Day19

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.List1
import Data.SortedMap as M
import Data.SortedSet as S
import Debug.Trace

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

data Color = White | Blue | Black | Red | Green

Show Color where
  show White = "w"
  show Blue  = "u"
  show Black = "b"
  show Red   = "r"
  show Green = "g"

Eq Color where
  White == White = True
  Blue  == Blue  = True
  Black == Black = True
  Red   == Red   = True
  Green == Green = True
  _     == _     = False

Ord Color where
  compare White Blue  = LT
  compare White Black = LT
  compare White Red   = LT
  compare White Green = LT
  compare Blue  Black = LT
  compare Blue  Red   = LT
  compare Blue  Green = LT
  compare Black Red   = LT
  compare Black Green = LT
  compare Red   Green = LT
  compare White White = EQ
  compare Blue  Blue  = EQ
  compare Black Black = EQ
  compare Red   Red   = EQ
  compare Green Green = EQ
  compare _     _     = GT

readColor : Char -> Color
readColor 'w' = White
readColor 'u' = Blue
readColor 'b' = Black
readColor 'r' = Red
readColor 'g' = Green
readColor  _  = White

Towel : Type
Towel = List Color

Pattern : Type
Pattern = List Color

parse : List (List String) -> (List Towel, List Pattern)
parse [(towels' :: _), patterns'] = (towels, patterns)
  where
    towels   = map (map readColor . unpack . trim) . forget . split (==',') $ towels'
    patterns = map (map readColor . unpack) $ patterns'
parse _ = ([],[])

countPatterns : List Towel -> SortedMap Pattern Int -> Pattern -> (Int, SortedMap Pattern Int)
countPatterns _      memo   []    = (1,memo)
countPatterns towels memo pattern with (lookup pattern memo)
  countPatterns towels memo pattern | Just tot = traceVal $ (tot,memo)
  countPatterns towels memo pattern | Nothing  = 
    let pts = [ countPatterns towels memo $ drop (length t) pattern
              | t <- towels, isPrefixOf t pattern]
        tot = sum $ map fst pts
        mem = foldl mergeLeft memo $ map snd pts
     in (tot, insert pattern tot mem)

process : List String -> Int
process input = let (towels, patterns) = parse . forget . split null $ input
                    available          = foldl (\m,p => snd $ countPatterns towels m p) empty patterns
                    patterns'          = S.fromList patterns
                    silver : Int
                    silver = cast . length $ filter (\(p,t) => (S.contains p patterns') && (t > 0)) $ M.toList available
                 in silver

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let silver = process $ lines content
                                     gold = 0
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day19.txt"
