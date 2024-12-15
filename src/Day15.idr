module Day15

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet as S

Coord : Type
Coord     = (Int,Int)
Boxes : Type
Boxes     = SortedSet (Int,Int)
Obstacles : Type
Obstacles = SortedSet (Int,Int)

Coord' : Type
Coord'     = (Int,Int,Int)
Boxes' : Type
Boxes'     = SortedSet (Int,Int,Int)

data Move = N | S | W | E

parseMoves : List Char -> List Move
parseMoves ('^' :: moves) = N :: parseMoves moves
parseMoves ('v' :: moves) = S :: parseMoves moves
parseMoves ('<' :: moves) = W :: parseMoves moves
parseMoves ('>' :: moves) = E :: parseMoves moves
parseMoves _              = []

parse : List (List String) -> (Obstacles, Boxes, Coord, List Move)
parse [grid',moves'] = (obstacles, boxes, robot, moves)
  where
    len : Int
    len   = cast . pred $ length grid'
    elems = foldr (++) []
           $ zipWith (\r,l => zipWith (\c,v => ((r,c),v)) [0 .. len] l)
                     [0 .. len] $ map unpack grid'
    obstacles = foldr (\(c,v),o => if v == '#' then insert c o else o) empty elems
    boxes     = foldr (\(c,v),b => if v == 'O' then insert c b else b) empty elems
    robot     = fromMaybe (0,0) . head' . map fst $ filter ((=='@') . snd) elems
    moves     = parseMoves . unpack $ joinBy "" moves'
parse _              = (empty,empty,(0,0), [])

move : Coord -> Move -> Coord
move (row, col) N = (row - 1, col)
move (row, col) S = (row + 1, col)
move (row, col) W = (row, col - 1)
move (row, col) E = (row, col + 1)

boxChain : Boxes -> Coord -> Move -> List Coord
boxChain bs c m with (move c m) | (contains (move c m) bs)
  boxChain bs c m | c' | True  = c' :: boxChain bs c' m
  boxChain bs c m | _  | False = []

sim : Obstacles -> Boxes -> Coord -> List Move -> (Boxes, Coord)
sim os bs c (m :: ms) with (contains (move c m) os) | (contains (move c m) bs)
  sim os bs c (m :: ms) | False | False = sim os bs (move c m) ms
  sim os bs c (m :: ms) | True  | _     = sim os bs c ms
  sim os bs c (m :: ms) | _     | True  = 
    let bc  = boxChain bs c m
        bc' = map (flip move m) $ toList bc
        bs' = foldr insert (foldr delete bs bc) bc'
        os' = contains (move (fromMaybe (0,0) $ last' bc) m) os
        c'  = move c m
     in if os' then sim os bs c ms
               else sim os bs' c' ms
sim _  bs c [] = (bs, c)

stretch : Coord -> Coord'
stretch (row,col) = (row, col * 2, col * 2 + 1)

stretch' : Coord -> List Coord
stretch' (row,col) = [(row, col * 2), (row, col * 2 + 1)]

unstretch : Coord' -> List Coord
unstretch (row,col1,col2) = [(row,col1), (row,col2)]

contains' : Coord -> Boxes' -> Bool
contains' c = contains c . S.fromList . concatMap unstretch . S.toList

boxChain' : Boxes' -> Move -> Coord' -> List Coord'
boxChain' bs W (r,c1,c2) with (contains (r,c1-2,c2-2) bs)
  boxChain' bs W (r,c1,c2) | True  = (r,c1-2,c2-2) :: boxChain' bs W (r,c1-2,c2-2)
  boxChain' bs W (r,c1,c2) | False = [(r,c1,c2)]
boxChain' bs E (r,c1,c2) with (contains (r,c1+2,c2+2) bs)
  boxChain' bs E (r,c1,c2) | True  = (r,c1+2,c2+2) :: boxChain' bs E (r,c1+2,c2+2)
  boxChain' bs E (r,c1,c2) | False = [(r,c1,c2)]
boxChain' bs N (r,c1,c2) with
    (any (flip contains bs) [(r-1,c1-1,c2-1),(r-1,c1,c2),(r-1,c1+1,c2+1)])
  boxChain' bs N (r,c1,c2) | True  = 
    let bxs = filter (flip contains bs) [(r-1,c1-1,c2-1),(r-1,c1,c2),(r-1,c1+1,c2+1)]
     in bxs ++ concatMap (boxChain' bs N) bxs
  boxChain' bs N (r,c1,c2) | False = []
boxChain' bs S (r,c1,c2) with
    (any (flip contains bs) [(r+1,c1-1,c2-1),(r+1,c1,c2),(r+1,c1+1,c2+1)])
  boxChain' bs S (r,c1,c2) | True  = 
    let bxs = filter (flip contains bs) [(r+1,c1-1,c2-1),(r+1,c1,c2),(r+1,c1+1,c2+1)]
     in bxs ++ concatMap (boxChain' bs S) bxs
  boxChain' bs S (r,c1,c2) | False = []

collision' : Coord -> Move -> Boxes' -> Bool
collision' (r,c) W bs = contains (r,c-1,c) bs
collision' (r,c) E bs = contains (r,c,c+1) bs
collision' (r,c) _ bs = any (flip contains bs) [(r,c-1,c),(r,c,c+1)]

move' : Coord' -> Move -> Coord'
move' (r, c1, c2) N = (r - 1, c1, c2)
move' (r, c1, c2) S = (r + 1, c1, c2)
move' (r, c1, c2) W = (r, c1 - 1, c2 - 1)
move' (r, c1, c2) E = (r, c1 + 1, c2 + 1)

sim' : Obstacles -> Boxes' -> Coord -> List Move -> (Boxes', Coord)
sim' os bs c (m :: ms) with (contains (move c m) os) | (collision' (move c m) m bs)
  sim' os bs c (m :: ms) | False | False = sim' os bs (move c m) ms
  sim' os bs c (m :: ms) | True  | _     = sim' os bs c ms
  sim' os bs c (m :: ms) | _     | True  = 
    let c'  = move c m
        bx  = if contains (fst c',snd c',snd c' + 1) bs
                 then (fst c',snd c',snd c' + 1)
                 else (fst c',snd c' - 1,snd c')
        bc  = nub . (bx ::) $ boxChain' bs m bx
        bc' = map (flip move' m) bc
        os' = intersection os . S.fromList $ concatMap (unstretch) bc'
        bs' = union (S.fromList bc') . difference bs $ S.fromList bc
     in if os' == empty
           then sim' os bs' c' ms
           else sim' os bs c ms
sim' _  bs c [] = (bs, c)

gps : (Int, Int) -> Int
gps (row,col) = 100 * row + col

gps' : (Int,Int,Int) -> Int
gps' (row,col,_) = 100 * row + col

process : List String -> (Int,Int)
process input = let (os, bs, r, ms) = parse . forget . split null $ input
                    (bs', r')       = sim os bs r ms
                    silver          = sum . map gps $ S.toList bs'
                    bss             = S.fromList . map stretch $ S.toList bs
                    oss             = concatMap (S.fromList . stretch')
                                    $ S.toList os
                    rs              = (fst r, (snd r) * 2) 
                    (bs'',r'')      = sim' oss bss rs ms
                    gold            = sum . map gps' $ S.toList bs''
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
    path = "./rsc/day15.txt"
