module Day21

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.SortedMap as M

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

pad : String -> SortedMap Char (Int,Int)
pad bs' with (unpack bs')
  _ | bs = let l = cast {to=Int} $ length bs
               p = [ (b, (i `mod` 3), (i `div` 3)) | (i,b) <- zip [0 .. l] bs]
            in M.fromList p

move : Int -> SortedMap Char (Int,Int) -> (Int,Int) -> List Char
     -> SortedMap (Int,Int,Bool) Int
move _ _   _      []     = empty
move n p (x,y) (b :: bs) with (M.lookup b p) | (M.lookup ' ' p)
  move n p (x,y) (b :: bs) | Just (x',y') | Just (bx,by) =
    let f = x' == bx && y == by || y' == by && x == bx
        r = M.fromList [((x' - x, y' - y, f), n)]
     in M.mergeWith (+) r $ move n p (x',y') bs
  move n p (x,y) (b :: bs) | _            | _            = empty

inst : Int -> Int -> Bool -> List Char
inst x y f = (if f then reverse s else s) ++ ['A']
  where
    l = unpack $ replicate (cast (-x)) '<'
    d = unpack $ replicate (cast y) 'v'
    u = unpack $ replicate (cast (-y)) '^'
    r = unpack $ replicate (cast x) '>'
    s = l ++ d ++ u ++ r

depth : Int -> SortedMap Char (Int,Int) -> SortedMap (Int,Int,Bool) Int -> Int
depth n p r with (M.lookup 'A' p)
  depth n p r | Nothing = 0
  depth n p r | Just i  =
    let d : SortedMap (Int,Int,Bool) Int -> SortedMap (Int,Int,Bool) Int
        d r' = foldr (M.mergeWith (+)) empty moves
          where
            moves = [ move n p i $ inst x y f | ((x,y,f),n) <- M.toList r'] 
     in sum . M.values $ foldl (\r',_ => d r') r [0 .. n]

unlock : Int -> SortedMap Char (Int,Int) -> SortedMap Char (Int,Int)
       -> (Int,Int) -> List (List Char) -> Int
unlock d np dp i cs = sum cp
  where
    ls = map (depth d dp) $ map (move 1 np i) cs
    cp = zipWith (\l,c => (l *) . readInt . pack $ take 3 c) ls cs

process : List String -> (Int,Int)
process input = (silver,gold)
  where
    codes  = map unpack input
    npad   = pad "789456123 0A"
    dpad   = pad " ^A<v>"
    init   = fromMaybe (2,3) $ M.lookup 'A' npad
    silver = unlock  2 npad dpad init codes
    gold   = unlock 25 npad dpad init codes

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day21.txt"
