module Day20

import System.File
import Data.String
import Data.Maybe
import Data.Nat
import Data.List
import Data.SortedMap as M
import Data.SortedSet as S

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

parse : List String -> (SortedMap (Int,Int) Char, (Int,Int), (Int,Int))
parse input = (M.fromList grid, start, end)
  where
    len  = cast {to=Int} . pred $ length input
    grid = foldr (++) []
         $ zipWith (\r,l => zipWith (\c,v => ((r,c),v)) [0 .. len] l)
                   [0 .. len] $ map unpack input
    start = fst . fromMaybe ((0,0),'S') $ find ((=='S') . snd) grid
    end   = fst . fromMaybe ((len,len), 'E') $ find ((=='E') . snd) grid

look : Int -> Int -> List (Int,Int)
look x y = [ (x + x', y + y')
           | (x', y') <- [(0,1), (1,0), (0,-1), (-1,0)] ]

look' : Int -> Int -> List (Int,Int)
look' x y = [ (x + 2*x', y + 2*y')
            | (x', y') <- [(0,1), (1,0), (0,-1), (-1,0)] ]

lookUp : SortedMap (Int,Int) Char -> (Int,Int) -> Char
lookUp m c = fromMaybe '#' $ M.lookup c m

lookUp' : SortedMap (Int,Int) Int -> (Int,Int) -> Int
lookUp' m c = fromMaybe 0 $ M.lookup c m

distances : SortedMap (Int,Int) Char -> SortedMap (Int,Int) Int
          -> SortedSet (Int,Int) -> SortedMap (Int,Int) Int
distances grid visited front with (front == empty)
  distances grid visited front | True  = visited
  distances grid visited front | False = 
    let dists    = map (lookUp' visited) $ S.toList front
        dmk      = S.keySet visited
        front'   = map ( filter ((/='#') . lookUp grid)
                       . filter (not . flip contains dmk) . uncurry look)
                 $ S.toList front
        visited' = M.mergeLeft visited . M.fromList . foldr (++) []
                 $ zipWith (\fs,d => map (\f => (f, d + 1)) fs) front' dists 
     in distances grid visited' . S.fromList $ foldr (++) [] front'

countCheats : List ((Int,Int), Int) -> SortedMap (Int,Int) Int -> Int
countCheats            []        _       = 0
countCheats (((r,c),d) :: dists) distMap = nchts + countCheats dists distMap
  where
    nn : (Int,Int) -> Int
    nn (r',c') with (M.lookup (r',c') distMap) 
      _ | Nothing = 0
      _ | Just d' = if ((d - d' - 2) >= 100) then 1 else 0
    nchts = sum . map nn $ look' r c

space : List (Int,Int)
space = [ (x', y') | x' <- [-20 .. 20], y' <- [-20 .. 20]
                   , ((abs x') + (abs y')) <= 20 ]

countCheats' : List ((Int,Int), Int) -> SortedMap (Int,Int) Int -> Int
countCheats'            []        _       = 0
countCheats' (((r,c),d) :: dists) distMap = nchts + countCheats' dists distMap
  where
    nn : (Int,Int) -> Int
    nn (r',c') with (M.lookup (r+r',c+c') distMap)
      _ | Nothing = 0
      _ | Just d' = if (d > d') && ((d - d' - (abs r') - (abs c')) >= 100)
                       then 1 else 0
    nchts = sum $ map nn space

process : List String -> (Int,Int)
process input = let (grid, start, end) = parse input
                    distMap = distances grid (M.singleton start 0)
                                             (S.singleton start)
                    silver  = countCheats  (M.toList distMap) distMap
                    gold    = countCheats' (M.toList distMap) distMap
                 in (silver,gold)

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day20.txt"
