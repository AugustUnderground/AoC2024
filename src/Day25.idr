module Day25

import System.File
import Data.String
import Data.Maybe
import Data.Nat
import Data.List
import Data.List1

Schematic : Type
Schematic  = List Int
Key : Type
Key        = List Int
Lock : Type
Lock       = List Int

parse' : List String -> (Char, Schematic)
parse' scm with (head' scm >>= (head' . unpack))
  parse' scm | Just '.' =
    let kscm = map (cast {to=Int} . pred . length . dropWhile (/='#'))
             . transpose $ map unpack scm
     in ('K', kscm)
  parse' scm | Just '#' = 
    let lscm = map (cast {to=Int} . pred . length . takeWhile (=='#'))
             . transpose $ map unpack scm
     in ('L', lscm)
  parse' scm | _        = ('X',[])

parse : List (List String) -> (List Key, List Lock)
parse input = (keys,locks)
  where
    scms  = map parse' input
    keys  = map snd $ filter ((=='K') . fst) scms
    locks = map snd $ filter ((=='L') . fst) scms

try : Lock -> Key -> Int 
try lock key with (all (<6) $ zipWith (+) lock key)
  _ | True  = 1
  _ | False = 0

process : List (List (String)) -> Int
process input = let (keys,locks) = parse input 
                    silver = sum [ try lock key | lock <- locks, key <- keys]
                 in silver

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let silver = process . forget . split null
                                            $ lines content
                                     gold   = "Chronicles delvired"
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day25.txt"
