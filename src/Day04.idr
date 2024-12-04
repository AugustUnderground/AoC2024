module Day04

import System.File
import Data.String
import Data.Maybe
import Data.SortedMap

asString : Maybe Char -> String
asString (Just c) = singleton c
asString Nothing  = ""

processInput : String -> List String -> Nat
processInput p ls = length $ filter (==p) u
  where
    i : Int
    i   = (cast $ length ls)
    j : Int
    j   = cast . length . fromMaybe "" $ head' ls
    idx = [ (i',j') | i' <- [0 .. (i - 1)], j' <- [0 .. (j - 1)] ]
    dct = fromList . zip idx . unpack $ joinBy "" ls
    ds : List (Int, Int)
    ds = [(x',y') | x' <- [(-1),0,1], y' <- [(-1),0,1]]
    u   = [ joinBy "" . map asString $ filter isJust [lookup (i'+x*n,j'+y*n) dct | n <- [0 .. 3]]
          | (x,y) <- ds, (i',j') <- keys dct ]

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let input = lines content
                                     silver  = show $ processInput pattern input
                                     gold    = show $ 0
                                  in putStrLn $ "Silver: " ++ silver
                                             ++ "\nGold: " ++ gold
                Left  error   => putStrLn (show error)
  where
    pattern = "XMAS"
    path    = "./rsc/day04.txt"
