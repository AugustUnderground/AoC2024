module Day13

import System.File
import Data.String
import Data.Maybe
import Data.List1

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

filterJust : List (Maybe a) -> List a
filterJust    []     = []
filterJust (x :: xs) with (x)
  filterJust (x :: xs) | Just x' = x' :: filterJust xs
  filterJust (x :: xs) | Nothing = filterJust xs

record Button where
  constructor MkButton
  dx, dy : Int

record Machine where
  constructor MkMachine
  a,b : Button
  x,y : Int

parse : List String -> Machine
parse [a,b,p] = MkMachine ba bb px py
  where
    dxa = readInt . pack . take 2 . drop 12 $ unpack a
    dya = readInt . pack . take 2 . drop 18 $ unpack a
    dxb = readInt . pack . take 2 . drop 12 $ unpack b
    dyb = readInt . pack . take 2 . drop 18 $ unpack b
    px  = readInt . pack . takeWhile (/=',') . drop 1 . dropWhile (/='=')
        $ unpack p
    py  = readInt . pack . drop 4 . dropWhile (/=',') $ unpack p
    ba  = MkButton dxa dya
    bb  = MkButton dxb dyb
parse _ = MkMachine (MkButton 0 0) (MkButton 0 0) 0 0

linalg' : Int -> Int -> Int -> Int -> Int -> Int -> Maybe Int
linalg' px py ax bx ay by with
    ((px * by - py * bx) `mod` (ax * by - ay * bx)) |
    ((py * ax - px * ay) `mod` (ax * by - ay * bx))
  linalg' px py ax bx ay by | 0 | 0 = 
    let a' = (px * by - py * bx) `div` (ax * by - ay * bx)
        b' = (py * ax - px * ay) `div` (ax * by - ay * bx)
     in Just (3 * a' + b')
  linalg' _  _  _  _  _  _  | _ | _ = Nothing

linalg : Machine -> Maybe Int
linalg m = linalg' x' y' ax bx ay by
  where
    x' = x m
    y' = y m
    bx = dx $ b m
    by = dy $ b m
    ax = dx $ a m
    ay = dy $ a m

convert : Machine -> Machine
convert m = MkMachine (a m) (b m) ((x m) + d) ((y m) + d)
  where
    d : Int
    d = 10000000000000 

process : List (List String) -> (Int,Int)
process input = (silver,gold)
  where
    machines = map parse input
    silver  = sum . filterJust $ map linalg machines
    gold    = sum . filterJust $ map (linalg . convert) machines

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process . forget
                                                   . split null $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day13.txt"
