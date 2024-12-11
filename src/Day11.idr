module Day11

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.SortedMap
import Debug.Trace

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

log10 : Double -> Double
log10 x = log x / log 10

nDig : Int -> Int
nDig x = cast . ceiling . log10 . cast $ x + 1

both : (a -> b) -> (a,a) -> (b,b) 
both f (x,y) = (f x, f y)

tl : (a,a) -> List a
tl (x,y) = [x,y]

splitDigs : Int -> (Int,Int)
splitDigs i = both (readInt . pack) . splitAt n $ unpack j
  where
    j = show i
    n : Nat
    n = cast $ (cast $ length j) `div` 2

blink' : Int -> SortedMap Int Int
blink' 0 = fromList [(0,-1), (1,1)]
blink' s with (mod (nDig s) 2 == 0)
  blink' s | True  = let (a,b) = splitDigs s
                      in fromList [(s,-1),(a,1),(b,1)]
  blink' s | False = fromList [(s * 2024, 1), (s,-1)]

merge' : Int -> Int -> Int
merge' a b = max 0 $ a + b

blink : Int -> SortedMap Int Int -> SortedMap Int Int
blink 0 stones = stones
blink n stones = blink (traceVal (n - 1)) stones'
  where
    foo = map blink' $ map fst . filter ((>0). snd) $ Data.SortedMap.toList stones
    stones' = foldl (mergeWith merge') stones $ traceVal foo

process : List Int -> (SortedMap Int Int,Int)
process stones = (silver,gold)
  where
    nums : List Int
    nums  = replicate (length stones) 1
    silver = blink 6 . fromList $ zip stones nums
    gold = sum $ values silver
    -- gold   = sum . values . blink 25 . fromList $ zip stones nums

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
    path = "./rsc/day11-example.txt"
