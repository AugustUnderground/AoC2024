module Day07

import System.File
import Data.String
import Data.List1
import Data.Maybe

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

split' : String -> (Int, List Int)
split' line = let (result ::: nums) = split (==':') line
                  operands = map readInt . words . fromMaybe "" . head' $ nums
               in (readInt result, operands)

(<|>) : Int -> Int -> Int
(<|>) o1 o2 = readInt $ show o1 ++ show o2

maximum : List Int -> Int
maximum = fromMaybe 0 . head' . reverse . sort

compute : List (Int -> Int -> Int) -> Int -> List Int -> Int
compute ops r [o1, o2] with (any (\op => (op o1 o2) == r) ops)
  compute ops r [o1, o2] | True  = r
  compute ops r [o1, o2] | False = 0
compute ops r (o1 :: o2 :: cs) = maximum [ compute ops r ((op o1 o2) :: cs)
                                         | op <- ops, (op o1 o2) <= r ]
compute _   _ _                = 0

process : List String -> (Int,Int)
process input = (silver,gold)
  where
    silver = sum $ map (uncurry (compute [(+),(*)]) . split') input
    gold   = sum $ map (uncurry (compute [(+),(*),(<|>)]) . split') input

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "Silver: " ++ show silver
                                             ++ "\nGold: " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day07.txt"
