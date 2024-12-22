module Day22

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.SortedMap as M

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

v : Int -> Int -> Int
v 0 0 = 0
v 1 1 = 0
v _ _ = 1

infix 7 ^
(^) : Int -> Int -> Int
(^) 0 0 = 0
(^) x 0 = x
(^) 0 y = y
(^) x y = b + 2 * r
  where
    b = (mod x 2) `v` (mod y 2)
    r = (div x 2)  ^  (div y 2)

infix 7 ///
(///) : Int -> Int -> Int
(///) = div

infix 7 /%/
(/%/) : Int -> Int -> Int
(/%/) x y with (x >= 10) | (y >= 10)
  (/%/) x y | True | True = mod x y
  (/%/) x y | _    | _    = x

prng : Int -> Int
prng x0 = x3
  where
    x1 = ((x0  *   64) ^ x0) /%/ 16777216
    x2 = ((x1 ///  32) ^ x1) /%/ 16777216
    x3 = ((x2  * 2048) ^ x2) /%/ 16777216

comp : Int -> (Int,Int)
comp v = (w,d)
  where
    w = prng v
    d = (w /%/ 10) - (v /%/ 10)

compute : Int -> List Int -> SortedMap (List Int) Int -> Int
        -> (Int, SortedMap (List Int) Int)
compute 0 cs se v = (v, se)
compute n cs se v with (length cs >= 3) 
  compute n cs se v | False =
    let (v',d) = comp v
        cs'    = d :: cs
        n'     = n - 1
     in compute n' cs' se v'
  compute n cs se v | True =
    let (v',d) = comp v
        cs'    = take 4 $ d :: cs
        o      = v' /%/ 10
        n'     = n - 1
        se'    = case (M.lookup cs' se) of 
                      Nothing => M.insert cs' o se
                      Just _  => se
     in compute n' cs' se' v'

process : List Int -> (Int,Int)
process input = (silver,gold)
  where
    res    = map (compute 2000 [] empty) input
    silver = sum $ map fst res
    gold   = fromMaybe 0 . last' . sort . M.values
           . foldr (M.mergeWith (+)) empty $ map snd res

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process . map readInt
                                                   $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day22.txt"
