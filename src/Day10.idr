module Day10

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.Nat
import Data.SortedSet
import Data.SortedMap
import Debug.Trace

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

look : Int -> Int -> Int -> List (Int,Int)
look lim row col = [ (r,c) | (r,c) <- [north,south,west,east]
                   , r <= lim, r >= 0, c <= lim, c >= 0 ]
  where
    north = (row - 1, col)
    south = (row + 1, col)
    west  = (row, col - 1)
    east  = (row, col + 1)

lookup' : List ((Int,Int),Int) -> Int -> Int -> Maybe ((Int,Int),Int)
lookup' grid row col with (lookup (row,col) grid)
  lookup' grid row col | Just height = Just ((row,col), height)
  lookup' grid row col | Nothing     = Nothing

walk : Int -> List ((Int,Int),Int) -> SortedSet (Int,Int) -> Int -> Int
    -> SortedMap (Int,Int) Int
walk lim grid stack row col with
    (look lim row col) | (fromMaybe 0 $ lookup (row,col) grid)
  walk lim grid stack row col | []   | _      = empty
  walk lim grid stack row col | _    | 0      = fromList [((row,col), 1)]
  walk lim grid stack row col | next | height = 
    let steps  = map fst
               . filter (\(rc,h) => (h==(height-1)) && not (contains rc stack))
               . catMaybes $ map (uncurry (lookup' grid)) next
        stack' = foldl (flip insert) stack steps
     in foldr mergeLeft empty $ map (uncurry (walk lim grid stack')) steps

walk' : Int -> List ((Int,Int),Int) -> SortedSet (Int,Int) -> Int -> Int -> Int
walk' lim grid stack row col with
    (look lim row col) | (fromMaybe 0 $ lookup (row,col) grid)
  walk' lim grid stack row col | []   | _      = 0
  walk' lim grid stack row col | _    | 9      = 1
  walk' lim grid stack row col | next | height = 
    let steps  = map fst
               . filter (\(rc,h) => (h==(height+1)) && not (contains rc stack))
               . catMaybes $ map (uncurry (lookup' grid)) next
        stack' = foldl (flip insert) stack steps
     in sum $ map (uncurry (walk' lim grid stack')) steps

process : List String -> (Int, Int)
process input = (scores,ratings)
  where
    grid'   = map (map (readInt . singleton) . unpack) input
    len : Int
    len     = cast . pred $ length grid'
    grid    = foldr (++) []
            $ zipWith (\r,l => zipWith (\c,v => ((r,c),v)) [0 .. len] l)
                      [0 .. len] grid'
    ths     = filter ((==0) . snd) grid
    tts     = filter ((==9) . snd) grid
    scores  = sum . values . foldl (mergeWith (+)) empty
            $ map (\((r,c),_) => walk len grid (fromList [(r,c)]) r c) tts
    ratings = sum
            $ map (\((r,c),_) => walk' len grid (fromList [(r,c)]) r c) ths

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day10.txt"
