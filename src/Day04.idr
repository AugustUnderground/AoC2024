module Day04

import System.File
import Data.String
import Data.Maybe
-- import Data.List
-- import Data.List.Elem
import Data.Nat
import Data.SortedMap

-- findx : String -> String -> Int
-- findx _ "" =  0
-- findx p l with (isPrefixOf p l) | (isPrefixOf (reverse p) l)
--   findx p l | True | _    = (+1) . findx p $ substr 1 (length l) l
--   findx p l | _    | True = (+1) . findx p $ substr 1 (length l) l
--   findx p l | _    | _    = (+0) . findx p $ substr 1 (length l) l

-- diagElem : List (List Char) -> Nat -> Nat -> Char
-- diagElem m r c = fromMaybe '-' . head' . drop c . fromMaybe [] . head' $ drop r m
-- 
-- findDiag : Nat -> Nat -> Nat -> List Char -> List (List Char) -> Int
-- findDiag len r c p css with (c < len) | (r < (length css))
--   findDiag len r c p css | True | True   = let cis = [c .. pred c + (length p)] 
--                                                ris = [r .. pred r + (length p)]
--                                                xms = pack $ zipWith (diagElem css) ris cis
--                                                xms' = pack $ zipWith (diagElem css) ris (reverse cis)
--                                                p' = pack p
--                                             in if (xms == p') || (xms' == p')
--                                                   || ( xms == (reverse p'))
--                                                   || ( xms' == (reverse p'))
--                                                   then 1 + findDiag len r (c + 1) p css
--                                                   else 0 + findDiag len r (c + 1) p css
--   findDiag len r c p css | False | _     = 0 + findDiag len (r + 1) 0 p css
--   findDiag len r c p css | _     | False = 0
--   findDiag len r c p css | _     | _     = 0

asString : Maybe Char -> String
asString (Just c) = singleton c
asString Nothing  = ""

processInput : String -> List String -> List ((Int, Int), Integer)
processInput p ls = u -- dct -- length $ filter (==p) x
  where
    i : Int
    i   = (cast $ length ls)
    j : Int
    j   = cast . length . fromMaybe "" $ head' ls
    idx = [ (i',j') | i' <- [0 .. (i - 1)], j' <- [0 .. (j - 1)] ]
    dct = fromList . zip idx . unpack $ joinBy "" ls
    ds : List (Int, Int)
    ds = [(x',y') | x' <- [(-1),0,1], y' <- [(-1),0,1]]
    u   = [ ((i',j'), n)
          -- [lookup (i'+x*n,j'+y*n) dct | n <- [0 .. 3]]
          -- joinBy "" . map asString
          -- $ filter isJust 
          | n <- [0 .. 3]
         -- , (x,y) <- ds
          , (i',j') <- keys dct
          --, (j'+y*n) > 0, (j'+y*n) < j
          --, (i'+x*n) > 0, (i'+x*n) < i
          ]

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


