module Day14

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.SortedSet

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

record Robot where
  constructor MkRobot
  px, py, vx, vy : Int

mkRobot : String -> Robot
mkRobot str' with (unpack str')
  _ | str = let x  = readInt . pack . takeWhile (/=',') $ drop 2 str
                y  = readInt . pack . takeWhile (/=' ') . drop 1
                   $ dropWhile (/=',') str
                x' = readInt . pack . takeWhile (/=',') . drop 2
                   $ dropWhile (/='v') str
                y' = readInt . pack . drop 1 . dropWhile (/=',')
                   $ dropWhile (/='v') str
             in MkRobot x y x' y'

sim : Int -> List Robot -> List Robot
sim _ [] = []
sim n (MkRobot x y x' y' :: rs) = MkRobot xx yy x' y' :: sim n rs
    where
      xx = mod (x + n * x') 101
      yy = mod (y + n * y') 103

quadrants : List Robot -> Int
quadrants rs = let (a,b,c,d) = foldl g (0,0,0,0) rs in (a * b * c * d)
  where
    x : Int
    x = 100 `div` 2
    y : Int
    y = 102 `div` 2
    f : (Int,Int,Int,Int) -> Robot -> (Int,Int,Int,Int)
    f (q1,q2,q3,q4) r with ((px r) > x) | ((py r) > y)
      f (q1,q2,q3,q4) r | True  | True  = (q1+1,q2,q3,q4)
      f (q1,q2,q3,q4) r | True  | False = (q1,q2+1,q3,q4)
      f (q1,q2,q3,q4) r | False | True  = (q1,q2,q3+1,q4)
      f (q1,q2,q3,q4) r | False | False = (q1,q2,q3,q4+1)
    g : (Int,Int,Int,Int) -> Robot -> (Int,Int,Int,Int)
    g q r with ((px r) /= x) | ((py r) /= y)
      g q r | True | True = f q r
      g q r | _    | _    = q

-- qmin : List Robot -> List Int -> Int -> Int -> Int
-- qmin _     []     n' s = n'
-- qmin rs (n :: ns) n' s with (quadrants $ sim n rs)
--   qmin rs (n :: ns) n' s | s' = if s' < s then qmin rs ns n s'
--                                           else qmin rs ns n' s

coords : Robot -> (Int,Int)
coords (MkRobot x y _ _) = (x,y)

qmin : List Robot -> Nat -> Int -> Int
qmin rs nr n with ((==nr) . length . nub . map coords $ sim n rs)
  qmin rs nr n | True  = n
  qmin rs nr n | False = qmin rs nr $ n + 1

mkTree : List Robot -> String
mkTree robots with (fromList $ map coords robots)
  mkTree robots | robs = unlines
                       [ pack [ (if contains (x,y) robs then '#' else ' ')
                         | x <- [0 .. 100] ] | y <- [0 .. 102]]

process : List String -> (Int,Int,String)
process input = (silver,gold,tree)
  where
    robots = map mkRobot input
    silver = quadrants $ sim 100 robots
    gold   = qmin robots (length robots) 1
    tree   = mkTree $ sim gold robots

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold,tree) = process
                                                        $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                                           ++ "\n\n"         ++ tree
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day14.txt"
