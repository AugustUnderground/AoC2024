module Day06

import System.File
import Data.String
import Data.Maybe
import Data.List1
import Data.Nat
import Data.SortedSet

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

data Direction = North | South | East | West

Eq Direction where
  North == North = True
  South == South = True
  East  == East  = True
  West  == West  = True
  _     == _     = False

Ord Direction where
  compare North South = LT
  compare North East  = LT
  compare North West  = LT
  compare South East  = LT
  compare South West  = LT
  compare East  West  = LT
  compare South North = LT
  compare East  North = GT
  compare West  North = GT
  compare East  South = GT
  compare West  South = GT
  compare West  East  = GT
  compare _ _         = EQ

Show Direction where
  show North = "North"
  show South = "South"
  show East  = "East"
  show West  = "West"

proc : (Int, List (Int, Char)) -> List (Int,Int)
proc (r, []) = []
proc (r, ((c,_) :: l)) = (r,c) :: proc (r, l)

patrol : Int -> (Int,Int) -> Direction -> List (Int,Int) -> List (Int,Int) -> List (Int,Int)
patrol lim (row,col) North obst stack with
    (head' . reverse . sort . map fst $ filter ((\(r',c') => (c' == col) && (r' < row))) obst)
  patrol lim (row,col) North obst stack | Just row' =
    let len : Nat
        len    = cast . abs $ row' - row
        stack' = stack ++ zip [row .. row' + 1] (replicate len col)
     in patrol lim (row' + 1,col) East obst stack'
  patrol lim (row,col) North obst stack | Nothing   =
    nub $ stack ++ zip [row .. 0] (replicate (cast $ row + 1) col)
patrol lim (row,col) South obst stack with
    (head' . sort . map fst $ filter ((\(r',c') => (c' == col) && (r' > row))) obst)
  patrol lim (row,col) South obst stack | Just row' =
    let len : Nat
        len    = cast . abs $ row' - row
        stack' = stack ++ zip [row .. row' - 1] (replicate len col)
     in patrol lim (row' - 1,col) West obst stack'
  patrol lim (row,col) South obst stack | Nothing   =
    nub $ stack ++ zip [row .. lim] (replicate (cast $ lim - row + 1) col)
patrol lim (row,col) East  obst stack with
    (head' . sort . map snd $ filter ((\(r',c') => (c' > col) && (r' == row))) obst)
  patrol lim (row,col) East  obst stack | Just col' =
    let len : Nat
        len    = (+1) . cast . abs $ col' - col
        stack' = stack ++ zip (replicate len row) [col .. col' - 1]
     in patrol lim (row,col' - 1) South obst stack'
  patrol lim (row,col) East  obst stack | Nothing   = 
    nub $ stack ++ zip (replicate (cast $ lim - col + 1) row) [col .. lim] 
patrol lim (row,col) West  obst stack with
  (head' . reverse . sort . map snd $ filter ((\(r',c') => (c' < col) && (r' == row))) obst)
  patrol lim (row,col) West  obst stack | Just col' =
    let len : Nat
        len    = cast . abs $ col' - col
        stack' = stack ++ zip (replicate len row) [col .. col' + 1]
     in patrol lim (row,col' + 1) North obst stack'
  patrol lim (row,col) West  obst stack | Nothing   = 
    nub $ stack ++ zip (replicate (cast $ col + 1) row) [col .. 0] 

patrol' : Int -> (Int, Int, Direction) -> SortedSet (Int,Int,Direction) -> List (Int,Int) -> Int
patrol' lim (row,col,North) visited obst with
    (head' . reverse . sort . map fst $ filter ((\(r',c') => (c' == col) && (r' < row))) obst)
  patrol' lim (row,col,North) visited obst | (Just row') with
      (contains (row' + 1, col, North) visited)
    patrol' lim (row,col,North) visited obst | (Just row') | True  = 1
    patrol' lim (row,col,North) visited obst | (Just row') | False =
       let visited' = insert (row' + 1, col, North) visited
        in patrol' lim (row' + 1,col,East) visited' obst 
  patrol' lim (row,col,North) visited obst | Nothing   = 0
patrol' lim (row,col,South) visited obst with
    (head' . sort . map fst $ filter ((\(r',c') => (c' == col) && (r' > row))) obst)
  patrol' lim (row,col,South) visited obst | (Just row') with
      (contains (row' - 1, col, South) visited)
    patrol' lim (row,col,South) visited obst | (Just row') | True  = 1
    patrol' lim (row,col,South) visited obst | (Just row') | False =
       let visited' = insert (row' - 1, col, South) visited
        in patrol' lim (row' - 1,col,West) visited' obst 
  patrol' lim (row,col,South) visited obst | Nothing   = 0
patrol' lim (row,col,East) visited obst with
    (head' . sort . map snd $ filter ((\(r',c') => (c' > col) && (r' == row))) obst)
  patrol' lim (row,col,East) visited obst | (Just col') with
      (contains (row, col' - 1, East) visited)
    patrol' lim (row,col,East) visited obst | (Just col') | True  = 1
    patrol' lim (row,col,East) visited obst | (Just col') | False =
       let visited' = insert (row, col' - 1, East) visited
        in patrol' lim (row,col' - 1,South) visited' obst 
  patrol' lim (row,col,East) visited obst | Nothing   = 0
patrol' lim (row,col,West) visited obst with
    (head' . reverse . sort . map snd $ filter ((\(r',c') => (c' < col) && (r' == row))) obst)
  patrol' lim (row,col,West) visited obst | (Just col') with
      (contains (row, col' + 1, West) visited)
    patrol' lim (row,col,West) visited obst | (Just col') | True  = 1
    patrol' lim (row,col,West) visited obst | (Just col') | False =
       let visited' = insert (row, col' + 1, West) visited
        in patrol' lim (row,col' + 1,North) visited' obst 
  patrol' lim (row,col,West) visited obst | Nothing   = 0

process : List String -> (Int, Int)
process ls = (silver,gold)
  where
    len : Int
    len        = cast . length . fromMaybe "" $ head' ls
    obstacles  = foldl (++) [] . map proc . filter (isCons . snd)
               . zip [0 .. (len - 1)]
               $ map (filter ((=='#') . snd) . zip [0 .. (len - 1)] . unpack) ls
    start      = fromMaybe (0,0) . head' . foldl (++) []
               . map proc . filter (isCons . snd) . zip [0 .. (len - 1)]
               $ map (filter ((=='^') . snd) . zip [0 .. (len - 1)] . unpack) ls
    route      = patrol (len - 1) start North obstacles []
    obstacles' = drop 1 route
    silver : Int
    silver     = cast $ length route
    row        = fst start
    col        = snd start
    gold : Int
    gold       = sum . map (patrol' (len - 1) (row,col,North) empty . (:: obstacles)) $ route

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day06.txt"
