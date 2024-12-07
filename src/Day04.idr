module Day04

import System.File
import Data.String
import Data.List
import Data.Maybe

findChar : Char -> List ((Int,Int),Char) -> List (Int,Int)
findChar chr = map fst . filter ((==chr) . snd)

walk' : List ((Int,Int),Char) -> String -> (Int,Int) -> (Int,Int) -> Int
walk' _ ("XMAS") _ _ = 1
walk' puzzle xmas (row,col) (dr,dc) with 
    (isPrefixOf xmas "XMAS") | (lookup (row+dr,col+dc) puzzle)
  walk' puzzle xmas (row,col) (dr,dc) | False | _        = 0
  walk' puzzle xmas (row,col) (dr,dc) | True  | Nothing  = 0
  walk' puzzle xmas (row,col) (dr,dc) | True  | Just chr =
    let xmas' = joinBy "" [xmas, cast chr]
     in walk' puzzle xmas' (row+dr,col+dc) (dr,dc) 

walk : List (Int,Int) -> List ((Int,Int),Char) -> Int
walk    []     _      = 0
walk (x :: xs) puzzle with (lookup x puzzle )
  walk (x :: xs) puzzle | Nothing  = 0
  walk (x :: xs) puzzle | Just chr =
    let directions : List (Int,Int)
        directions = [(1,0),(0,1),(-1,0),(0,-1),(1,1),(-1,-1),(-1,1),(1,-1)]
        xmas = sum $ map (walk' puzzle (cast chr) x) directions
     in xmas + walk xs puzzle

lookup' : List ((Int,Int),Char) -> (Int,Int) -> Char
lookup' p rc with (lookup rc p)
  lookup' p rc | Just c  = c
  lookup' p rc | Nothing = 'X'

findX' : List ((Int,Int),Char) -> (Int,Int) -> (Int,Int) -> Int
findX' puzzle (row,col) (dr,dc) = 0

findX : (Int,Int) -> List ((Int,Int),Char) -> Bool
findX (row,col) puzzle = any (==lr) options && any (==rl) options
  where
    options : List String
    options = ["MAS", "SAM"]
    lr = pack . intersperse 'A'
       $ map (lookup' puzzle . (\(r,c) => (row+r,col+c))) [(1,1),(-1,-1)]
    rl = pack . intersperse 'A'
       $ map (lookup' puzzle . (\(r,c) => (row+r,col+c))) [(-1,1),(1,-1)]

findXs : List (Int,Int) -> List ((Int,Int),Char) -> Int
findXs    []     _      = 0
findXs (a :: as) puzzle with (findX a puzzle)
  findXs (a :: as) puzzle | False = 0 + findXs as puzzle
  findXs (a :: as) puzzle | True  = 1 + findXs as puzzle

process : List String -> (Int,Int)
process (ls@(l :: _)) = (silver,gold)
  where
    len : Int
    len    = cast $ length l
    puz    = foldl (++) []
           $ zipWith (\row,line => zipWith (\col,chr => ((row,col),chr))
                                           [0 .. len] (unpack line))
                     [0 .. len] ls
    xs     = findChar 'X' puz
    as     = findChar 'A' puz
    silver = walk xs puz
    gold   = findXs as puz
process [] = (0,0)

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "Silver: " ++ show silver
                                             ++ "\nGold: " ++ show gold
                Left  error   => putStrLn (show error)
  where
    pattern = "XMAS"
    path    = "./rsc/day04.txt"
