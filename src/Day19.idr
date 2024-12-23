module Day19

import System.File
import Data.String
import Data.List
import Data.List1
import Data.SortedMap as M
import Control.Monad.State

data Color = White | Blue | Black | Red | Green

Eq Color where
  White == White = True
  Blue  == Blue  = True
  Black == Black = True
  Red   == Red   = True
  Green == Green = True
  _     == _     = False

Ord Color where
  compare White Blue  = LT
  compare White Black = LT
  compare White Red   = LT
  compare White Green = LT
  compare Blue  Black = LT
  compare Blue  Red   = LT
  compare Blue  Green = LT
  compare Black Red   = LT
  compare Black Green = LT
  compare Red   Green = LT
  compare White White = EQ
  compare Blue  Blue  = EQ
  compare Black Black = EQ
  compare Red   Red   = EQ
  compare Green Green = EQ
  compare _     _     = GT

readColor : Char -> Color
readColor 'w' = White
readColor 'u' = Blue
readColor 'b' = Black
readColor 'r' = Red
readColor 'g' = Green
readColor  _  = White

Towel : Type
Towel = List Color

Pattern : Type
Pattern = List Color

parse : List (List String) -> (List Towel, List Pattern)
parse [(towels' :: _), designs'] = (towels, designs)
  where
    towels   = map (map readColor . unpack . trim) . forget . split (==',')
             $ towels'
    designs = map (map readColor . unpack) $ designs'
parse _ = ([],[])

count' : Pattern -> List Towel -> State (SortedMap Pattern Int) Int
count' pattern towels = do
  cache <- get
  case (M.lookup pattern cache) of
       Just cnt => pure cnt
       Nothing  => do
         let valid = filter (flip isPrefixOf pattern) towels
         tot <- foldlM (\c,d => (c+) <$> count' (drop (length d) pattern) towels)
                       0 valid
         cache' <- get
         put $ M.insert pattern tot cache'
         pure tot

count : List Pattern -> List Towel -> State (SortedMap Pattern Int) (List Int)
count          []           towels = pure []
count (pattern :: patterns) towels = do
  cnt  <- count' pattern towels
  (cnt ::) <$> count patterns towels

process : List String -> (Int,Int)
process input = let (towels, designs) = parse . forget . split null $ input
                    cache             = M.fromList [([],1)]
                    achievable        = evalState cache (count designs towels)
                    silver            = cast {to=Int} . length
                                      $ filter (>0) achievable
                    gold              = sum achievable
                 in (silver,gold)

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day19.txt"
