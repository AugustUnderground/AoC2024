module Day12

import System.File
import Data.String
import Data.Maybe
import Data.Nat
import Data.List1
import Data.SortedMap as M
import Data.SortedSet as S
import Debug.Trace

contains : SortedMap k v -> k -> Bool
contains m k with (lookup k m)
  contains m k | Just _  = True
  contains m k | Nothing = False

look : Int -> Int -> List (Int,Int)
look row col = [ (r,c) | (r,c) <- [north,south,west,east] ]
  where
    north = (row - 1, col)
    south = (row + 1, col)
    west  = (row, col - 1)
    east  = (row, col + 1)

flood' : SortedMap (Int,Int) Char -> SortedMap (Int,Int) Char
       -> SortedMap (Int,Int) Char -> Int -> Int
       -> (Int,Int, SortedMap (Int,Int) Char)
flood' garden flooded stack area perim with (leftMost stack)
  flood' garden flooded stack area perim | Nothing                 = (area,perim,flooded)
  flood' garden flooded stack area perim | Just ((row,col), plant) =
    let valid : (Int,Int) -> Bool
        valid p = (not $ contains flooded p) 
                && (plant == (fromMaybe '-' $ lookup p garden))
        valid' : (Int,Int) -> Bool
        valid' p = (not $ contains garden p) 
                || (plant /= (fromMaybe '-' $ lookup p garden))
        plots  = look row col
        stack' = foldl (\m,rc => insert rc plant m) (delete (row,col) stack)
               $ filter valid plots
        perim' : Int
        perim' = (+perim) . cast . length $ filter valid' plots
        area' = area + 1
        flooded' = insert (row,col) plant flooded
     in flood' garden flooded' stack' area' perim'

flood : SortedMap (Int,Int) Char -> List (Int,Int)
flood garden with (leftMost garden) 
  flood garden | Nothing        = []
  flood garden | Just ((r,c),p) =
    let stack = fromList [((r,c),p)]
        (area,perim,flooded) = flood' garden empty stack 0 0
        in ((area,perim) ::) . flood . foldl (flip delete) garden $ keys flooded

look' : Int -> Int -> List ((Int,Int), (Int,Int))
look' r c = [ ((dr,dc), (r+dr,c+dc) ) | (dr,dc) <- [(1,0),(0,1),(-1,0),(0,-1)] ]

findSides : List (SortedSet (Int,Int)) -> Int
findSides          []        = 0
findSides (rcs :: perim) = sides + findSides perim
  where
    walk' : List (Int,Int) -> SortedSet (Int,Int) -> SortedSet (Int,Int)
    walk' [] seen = seen
    walk' ((r,c) :: rcs') seen with (S.contains (r,c) seen)
      walk' ((r,c) :: rcs') seen | True = walk' rcs' seen
      walk' ((r,c) :: rcs') seen | False = 
        let seen' = S.insert (r,c) seen
            rcs'' = [ (r+dr,c+dc) | (dr,dc) <- [(1,0),(0,1),(-1,0),(0,-1)]
                  , S.contains (r+dr,c+dc) rcs ]
         in walk' (nub $ rcs' ++ rcs'') seen'
    walk : List (Int,Int) -> SortedSet (Int,Int) -> Int
    walk [] _ = 0
    walk ((r,c) :: rcs') seen with (S.contains (r,c) seen)
      walk ((r,c) :: rcs') seen | True  = walk rcs' seen
      walk ((r,c) :: rcs') seen | False = let seen' = walk' [(r,c)] seen
                                           in 1 + walk rcs' seen'
    sides = walk (S.toList rcs) S.empty

fill' : SortedMap (Int,Int) Char -> SortedMap (Int,Int) Char
      -> SortedMap (Int,Int) Char -> Int -> SortedMap (Int,Int) (SortedSet (Int,Int))
      -> (Int,Int, SortedMap (Int,Int) Char)
fill' garden flooded stack area perim with (leftMost stack)
  fill' garden flooded stack area perim | Nothing = 
    let sides = findSides $ M.values perim
     in (area,sides,flooded)
  fill' garden flooded stack area perim | Just ((row,col), plant) =
    let valid : (Int,Int) -> Bool
        valid p = (not $ contains flooded p) &&
                  (plant == (fromMaybe '-' $ lookup p garden))
        valid' : (Int,Int) -> Bool
        valid' p = (not $ contains garden p) 
                || (plant /= (fromMaybe '-' $ lookup p garden))
        plots  = look' row col
        stack' = foldl (\m,rc => insert rc plant m) (delete (row,col) stack)
               . filter valid $ map snd plots
        area'  = area + 1
        rc = S.fromList [(row,col)]
        sides  =  M.fromList . map (\(dir,_) => (dir, rc))
               $ filter (valid' . snd) plots
        perim' = M.mergeWith S.union perim sides
        flooded' = insert (row,col) plant flooded
     in fill' garden flooded' stack' area' perim'

fill : SortedMap (Int,Int) Char -> List (Int,Int)
fill garden with (leftMost garden) 
  fill garden | Nothing    = []
  fill garden | Just ((r,c),p) =
    let stack = M.fromList [((r,c),p)]
        (area,perim,flooded) = fill' garden empty stack 0 M.empty 
        in ((area,perim) ::) . fill . foldl (flip delete) garden $ keys flooded

process : List (List Char) -> (Int,Int)
process input = (silver,gold)
  where
    len : Int
    len    = cast . pred $ length input
    garden = M.fromList . foldr (++) []
           $ zipWith (\r,l => zipWith (\c,v => ((r,c),v)) [0 .. len] l)
                     [0 .. len] input
    silver = sum $ map (uncurry (*)) $ flood garden
    gold   = sum $ map (uncurry (*)) $ fill garden

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process . map unpack
                                                   $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day12.txt"
