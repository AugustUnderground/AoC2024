module Day08

import System.File
import Data.String
import Data.Maybe
import Data.List

proc : (Int, List (Int, Char)) -> List ((Int,Int),Char)
proc (r, []) = []
proc (r, ((c,a) :: l)) = ((r,c),a) :: proc (r, l)

pairs' : List (Int,Int) -> List ((Int,Int),(Int,Int))
pairs' [] = []
pairs' ants@(a :: as) with (length ants > 1)
  pairs' ants@(a :: as) | False = []
  pairs' ants@(a :: as) | True  = [(a,b) | b <- as] ++ pairs' as

pairs : List ((Int,Int),Char) -> Char -> (Char,List ((Int,Int),(Int,Int)))
pairs antennas freq = (freq, pairs' locs)
  where
    locs = map fst $ filter ((==freq) . snd) antennas

midpoint : Int -> (Int,Int) -> (Int,Int) -> List (Int,Int)
midpoint lim (r1,c1) (r2,c2) = [ (r,c) | (r,c) <- [(r3,c3),(r4,c4)]
                               , r >= 0, r < lim, c >= 0, c < lim ]
  where
    r3 = 2 * r1 - r2
    c3 = 2 * c1 - c2
    r4 = 2 * r2 - r1
    c4 = 2 * c2 - c1

harmonics : Int -> (Int,Int) -> (Int,Int) -> List (Int,Int)
harmonics lim a1@(r1,c1) a2@(r2,c2) = nub $ [a1,a2]
                                   ++ iterate (step (δr,δc)) (r2,c2)
                                   ++ iterate (step (-δr,-δc)) (r1,c1)
  where
    δr = r1 - r2
    δc = c1 - c2
    step : (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
    step (dr,dc) (r,c) with ( r + dr >= 0 && r + dr < lim
                            && c + dc >= 0 && c + dc < lim )
      step (dr,dc) (r,c) | True  = Just (r + dr, c + dc)
      step (dr,dc) (r,c) | False = Nothing

process : List String -> (Int,Int)
process ls = (silver,gold)
  where
    len : Int
    len       = cast . length . fromMaybe "" $ head' ls
    antennas  = foldl (++) [] . map proc . filter (isCons . snd)
              . zip [0 .. (len - 1)]
              $ map (filter ((/='.') . snd) . zip [0 .. (len - 1)] . unpack) ls
    locs      = map (pairs antennas) . nub $ map snd antennas 
    antinodes = nub . foldl (++) []
              $ map (foldl (++) [] . map (uncurry (midpoint len)) . snd) locs
    silver : Int
    silver    = cast $ length antinodes
    harmonic  = nub . foldl (++) [] 
              $ map (foldl (++) [] . map (uncurry (harmonics len)) . snd) locs
    gold : Int
    gold      = cast $ length harmonic 

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day08.txt"
