module Day11

import System.File
import Data.String
import Data.Maybe
import Data.SortedMap as Map

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

log10 : Double -> Double
log10 x = log x / log 10

nDig : Int -> Int
nDig x = cast . ceiling . log10 . cast $ x + 1

both : (a -> b) -> (a,a) -> (b,b) 
both f (x,y) = (f x, f y)

splitDigs : Int -> (Int,Int)
splitDigs i = both (readInt . pack) . splitAt n $ unpack j
  where
    j = show i
    n : Nat
    n = cast $ (cast $ length j) `div` 2

blink' : Int -> Int -> SortedMap Int Int
blink' 0 n = fromList [(1, n)]
blink' s n with (mod (nDig s) 2 == 0)
  blink' s n | True  = let (a,b) = splitDigs s
                      in if a == b then fromList [(a, 2 * n)]
                                   else fromList [(a, n), (b, n)]
  blink' s n | False = fromList [(s * 2024, n)]

blink : Int -> SortedMap Int Int -> SortedMap Int Int
blink 0 stones = stones
blink n stones = blink (n - 1) . foldl (mergeWith (+)) empty
               . map (uncurry blink') $ Map.toList stones

observe : List Int -> Int -> Int
observe s n = sum . values . blink n . fromList $ zip s $ replicate (length s) 1

process : List Int -> (Int, Int)
process stones = (silver,gold)
  where
    silver = observe stones 25
    gold   = observe stones 75

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process . map readInt
                                                   . words $ trim content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day11.txt"
