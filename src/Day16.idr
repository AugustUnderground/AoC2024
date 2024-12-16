module Day16

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.Nat
import Data.SortedSet as S
import Debug.Trace

data Dir = N | S | W | E

Eq Dir where
  N == N = True
  S == S = True
  W == W = True
  E == E = True
  _ == _ = False

Show Dir where
  show N = "^"
  show S = "v"
  show W = "<"
  show E = ">"

parse : List String -> (SortedSet (Int,Int), (Int,Int), (Int,Int))
parse grid = (insert end tiles, start, end)
  where
    len : Int
    len   = cast . pred $ length grid
    elems = foldr (++) []
           $ zipWith (\r,l => zipWith (\c,v => ((r,c),v)) [0 .. len] l)
                     [0 .. len] $ map unpack grid
    tiles = S.fromList . map fst $ filter ((=='.') . snd) elems
    start = fromMaybe (0,0) . head' . map fst $ filter ((=='S') . snd) elems
    end   = fromMaybe (0,0) . head' . map fst $ filter ((=='E') . snd) elems

look : (Int, Int) -> List (Dir, (Int,Int))
look (r,c) = [ (d, (r+x,c+y)) | (d, (x,y)) <- [(E, (0,1)),(S, (1,0)),(N, (-1,0)),(W, (0,-1))]]

cost : Dir -> Dir -> Int
cost d1 d2 with (d1 == d2)
  _ | True  = 1
  _ | False = 1001

cost' : (Int,((Int,Int),Dir)) -> (Int,((Int,Int),Dir)) -> Ordering
cost' (c1, _) (c2, _) with (c1 < c2) | (c1 > c2)
  _ | True | _    = LT
  _ | _    | True = GT
  _ | _    | _    = EQ

djk : SortedSet (Int,Int) -> (Int,Int) -> SortedSet (Int,Int)
  -> List (Int,((Int,Int),Dir)) -> (Int, SortedSet (Int,Int))
djk grid _      seen [] = (0,seen)
djk grid target seen ((c,(s,d)) :: stack) with (s == target)
  djk grid target seen ((c,(s,d)) :: stack) | True  = (c, insert s seen)
  djk grid target seen ((c,(s,d)) :: stack) | False =
    let moves  = map (\(d',c') => (c + cost d d', (c', d')))
               . filter (not . flip contains seen . snd)
               . filter (flip contains grid . snd) $ look s
        seen'  = insert s seen
        stack' = sortBy cost' $ moves ++ stack
     in djk grid target seen' stack'

bt : SortedSet (Int,Int) -> SortedSet (Int,Int) -> (Int,Int) -> Int -> List (Dir, (Int,Int)) -> (SortedSet (Int,Int), List (Int,Int))
bt seen branches target kost [] = (branches, [])
bt seen branches target kost path@((d,loc) :: _) with
    (filter (flip contains seen . snd) . nub . look $ loc)
  bt seen branches target kost path@((d,loc) :: _) | []  = (branches, [])
  bt seen branches target kost path@((d,loc) :: _) | nxt =
    let seen' = foldr delete seen $ map snd nxt
        next = filter (not . flip contains branches . snd) nxt
        bar : List Dir -> Int
        bar [] = 0
        bar (d :: d' :: ds) = if d == d' then 1 + bar (d' :: ds)
                                         else 1001 + bar (d' :: ds)
        bar [_] = 1
     in case next of
             [] => traceVal (branches, [])
             (n :: _) => let branches' = insert (snd n) branches
                          in if (target == snd n)
                             then let path' = reverse $ nub $ (d,target) :: path
                                      cst = bar . map Builtin.fst $ path'
                                   in if (cst == kost) then (branches', map snd path') else (branches',[])
                             else bt seen' branches' target kost (n :: path) 

lee : SortedSet (Int,Int) -> SortedSet (Int,Int) -> Int -> List (Int, ((Int,Int), (Dir,Int))) -> Int
lee grid seen kost [] = 0 
lee grid seen kost wave@((i,_) :: _) = 0 
  where
    front' = takeWhile ((==i) . fst) wave
    foo = map (\(_,(l,(d,c))) => map look l) front'

process : List String -> (Int,Int)
process input = let (grid, start, end) = parse input
                    (silver, seen) = djk grid end empty [(0,(start,E))]
                    foo : (SortedSet (Int,Int), a) -> Maybe (SortedSet (Int,Int), List (Int,Int))
                    foo (bs,_) = let (bs',p) = bt seen bs end (silver + 1) [(E,start)] 
                                  in if bs' == empty then Nothing
                                                 else Just (bs', p)
                    paths          = iterate foo (empty, [])
                    gold : Int
                    gold = cast $ length paths
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
    path = "./rsc/day16-example.txt"
