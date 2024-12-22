module Day16

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.Nat
import Data.SortedSet as S
import Data.SortedMap as M

parse : List String -> (SortedSet (Int,Int), (Int,Int), (Int,Int))
parse maze = (insert end tiles, start, end)
  where
    len : Int
    len   = cast . pred $ length maze
    elems = foldr (++) []
           $ zipWith (\r,l => zipWith (\c,v => ((r,c),v)) [0 .. len] l)
                     [0 .. len] $ map unpack maze
    tiles = S.fromList . map fst $ filter ((=='.') . snd) elems
    start = fromMaybe (0,0) . head' . map fst $ filter ((=='S') . snd) elems
    end   = fromMaybe (0,0) . head' . map fst $ filter ((=='E') . snd) elems

score : (Int,Int) -> (Int,Int) -> Int
score ( 1, 0) ( 1, 0) =    1
score ( 1, 0) ( 0,-1) = 1001
score ( 1, 0) ( 0, 1) = 1001
score ( 0,-1) ( 0,-1) =    1
score ( 0,-1) ( 1, 0) = 1001
score ( 0,-1) (-1, 0) = 1001
score ( 0, 1) ( 0, 1) =    1
score ( 0, 1) ( 1, 0) = 1001
score ( 0, 1) (-1, 0) = 1001
score (-1, 0) (-1, 0) =    1
score (-1, 0) ( 0,-1) = 1001
score (-1, 0) ( 0, 1) = 1001
score    _       _    = 2001

record Position where
  constructor Pos
  cst : Int
  pos : (Int,Int)
  dir : (Int,Int)
  pth : SortedSet (Int,Int)

Eq Position where
  (Pos c1 o1 d1 p1) == (Pos c2 o2 d2 p2) =
    (c1 == c2) && (o1 == o2) && (d1 == d2) && (p1 == p2)

Ord Position where
  compare (Pos c1 _ _ _) (Pos c2 _ _ _) with (c1 < c2) | (c1 > c2)
    _ | True | _    = LT
    _ | _    | True = GT
    _ | _    | _    = EQ

Stack : Type
Stack = List (Position)

orient : (Int,Int) -> (Int,Int) -> (Int,Int)
orient (x, y) (x', y') = (x' - x, y' - y)

adj : (Int,Int) -> (Int,Int) -> List (Int,Int)
adj (x,y) ( 0, 1) = [(x + 1, y), (x - 1, y), (x, y + 1)]
adj (x,y) ( 0,-1) = [(x + 1, y), (x - 1, y), (x, y - 1)]
adj (x,y) ( 1, 0) = [(x + 1, y), (x, y + 1), (x, y - 1)]
adj (x,y) (-1, 0) = [(x - 1, y), (x, y + 1), (x, y - 1)]
adj (x,y)    _    = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

po : (Int, a) -> (Int, a) -> Ordering
po (a, _) (b, _) with (a < b) | (a > b)
  _ | True | _    = LT
  _ | _    | True = GT
  _ | _    | _    = EQ

chkp : List (Int, a) -> Int -> Bool
chkp          []    _ = False
chkp ((c', _) :: _) c = c' < c

djk : SortedSet (Int,Int) -> (Int,Int) -> Stack -> SortedMap ((Int,Int),(Int,Int)) Int
    -> List (Int, SortedSet (Int,Int)) -> List (Int, SortedSet (Int,Int))
djk _    _                     []        _       paths                     = paths
djk maze target ((Pos c p d t) :: stack) visited paths with
    (p == target) | (M.lookup (p,d) visited)
  djk maze target ((Pos c p d t) :: stack) visited paths | True  | _       =
    djk maze target stack visited . sortBy po $ (c, t) :: paths
  djk maze target ((Pos c p d t) :: stack) visited paths | False | Nothing = 
    let visited' = M.insert (p,d) c visited
     in djk maze target ((Pos c p d t) :: stack) visited' paths
  djk maze target ((Pos c p d t) :: stack) visited paths | False | Just c' =
    if (c' < c) || (chkp paths c)
       then djk maze target stack visited paths
       else let visited' = M.insert (p,d) c visited
                valid    = filter (flip S.contains maze) $ adj p d
                mp : (Int,Int) -> Position
                mp p' = Pos c' p' d' t'
                  where
                    d' = orient p p'
                    c' = c + score d d'
                    t' = S.insert p' t
                stack'   = stack ++ map mp valid
             in djk maze target stack' visited' paths

process : List String -> (Int,Int)
process input = let (maze, start, end) = parse input
                    stack  = [(Pos 0 start (0,1) (S.singleton start))]
                    paths  = djk maze end stack empty []
                    silver = fromMaybe 0 . head' . sort $ map fst paths
                    gold   = cast {to=Int} . length . S.toList
                           . foldr S.union empty . map snd
                           $ filter ((==silver) . fst) paths
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
    path = "./rsc/day16.txt"
