module Day05

import System.File
import Data.String
import Data.Maybe
import Data.List1
import Data.Nat

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

both' : (Bool,Bool) -> Bool
both' (True,True) = True
both' _           = False

both : (a -> b) -> (a,a) -> (b,b) 
both f (x,y) = (f x, f y)

right : (a -> b) -> (a,a) -> (a,b)
right f (x,y) = (x, f y)

split' : List String -> (List (Int, Int), List (List Int))
split' ls = (o,u)
  where
    s = split null ls
    o = map (both readInt . right (substr 1 2) . break ('|'==)) $ head s
    u = map (map readInt . forget . split (==',')) . fromMaybe [] . head'
      $ tail s

checkOrder : List (Int, Int) -> List Int -> Bool
checkOrder order update = all ((\(a,b) => a < b) . both (fromMaybe 0))
                        . filter (both' . both isJust)
                        $ map (\(a,b) => ((lookup a idx),(lookup b idx))) order
  where
    n : Int
    n   = cast . pred $ length update
    idx = zip update [0 .. n]

middle : List Int -> Int
middle update = fromMaybe 0 . head' . drop idx $ update
  where
    idx : Nat
    idx = cast . flip div 2 . cast $ length update

compare : List (Int, Int) -> Int -> Int -> Ordering
compare order x y with ((x, y) `elem` order) | ((y, x) `elem` order)
  compare order x y | True | _    =  LT
  compare order x y | _    | True =  GT
  compare order x y | _    | _    =  EQ

process : List (Int,Int) -> List (List Int) -> (Int,Int)
process ordering updates = (midSum,midSum')
  where
    checked  = zip updates $ map (checkOrder ordering) updates
    midSum   = sum . map (middle . fst) . filter snd $ checked
    midSum'  = sum . map (middle . sortBy (compare ordering) . fst)
             . filter (not . snd) $ checked

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (ordering,updates) = split' $ lines content
                                     (silver,gold)      = process ordering updates
                                  in putStrLn $ "Silver: " ++ show silver
                                             ++ "\nGold: " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day05.txt"
