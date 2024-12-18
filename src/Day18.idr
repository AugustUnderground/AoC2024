module Day18

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.SortedSet as S

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

parse : String -> (Int,Int)
parse str with (unpack str)
  _ | str' = let x = readInt . pack . takeWhile (/=',') $ str'
                 y = readInt . pack . drop 1 . dropWhile (/=',') $ str'
              in (x,y)

look : Int -> Int -> List (Int,Int)
look x y = [ (x + x', y + y') | (x', y') <- [(0,1), (1,0), (0,-1), (-1,0)] ]

lee : SortedSet (Int,Int) -> SortedSet (Int,Int) -> (Int,Int) -> Int
    -> SortedSet (Int,Int) -> Maybe Int
lee grid visited target step front with (contains target front) | (front == empty)
  lee grid visited target step front | _     | True  = Nothing
  lee grid visited target step front | True  | False = Just step
  lee grid visited target step front | False | False = 
    let front'   = flip intersection grid . flip difference visited
                 . fromList . concatMap (uncurry look) $ S.toList front
        step'    = step + 1
        visited' = union visited front
     in lee grid visited' target step' front'

lee' : Int -> List (Int,Int) -> (Int,Int)
lee' lim corr with (reverse . drop 1 . reverse $ corr)
  lee' lim corr | corr' =
    let grid  = flip difference (S.fromList corr')
              $ fromList [ (x,y) | x <- [0 .. lim], y <- [0 .. lim] ]
        final = fromMaybe (0,0) $ last' corr
     in case (lee grid empty (lim, lim) 0 $ S.singleton (0,0)) of
             Just _  => final
             Nothing => lee' lim . reverse . drop 1 $ reverse corr

process : List String -> (Int,(Int,Int))
process input = (silver,gold)
  where
    num : Nat
    num    = 1024
    lim : Int
    lim    = 70
    corr   = map parse input
    corr'  = fromList . take num $ corr
    grid   = flip difference corr'
           $ fromList [ (x,y) | x <- [0 .. lim], y <- [0 .. lim] ]
    silver = fromMaybe 0 . lee grid empty (lim, lim) 0 $ S.singleton (0,0)
    gold   = lee' lim corr

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show (fst gold)
                                                      ++ "," ++ show (snd gold)
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day18.txt"
