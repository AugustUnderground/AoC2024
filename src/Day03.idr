module Day03

import System.File
import Data.String
import Data.Maybe
import Data.List1

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

readLast :  List (List Char) -> Int
readLast strs = if null str then 0 else readInt str
  where
    str = pack . fromMaybe [] . head' $ strs

process : String -> Int
process program with (isPrefixOf "mul(" program) | (null program)
  process program | True | False = let code = split (==',') . takeWhile (/=')')
                                            . drop 4 $ unpack program
                                       m    = cast $ length program
                                       a    = readInt . pack $ head code
                                       b    = readLast $ tail code
                                    in ((a * b) +) . process $ substr 1 m program
  process program | False | False = process $ substr 1 (cast $ length program) program
  process program | _     | True  = 0

process' : Bool -> String -> Int
process' _    ""                       = 0
process' mult program with (isPrefixOf "don't(" program) | (isPrefixOf "do(" program)
  process' mult program | True  | _    = let m = cast $ length program
                                          in process' False $ substr 6 m program
  process' mult program | _     | True = let m = cast $ length program
                                          in process' True $ substr 3 m program
  process' mult program | _     | _ = if (isPrefixOf "mul(" program)
                                         then let code = split (==',') . takeWhile (/=')')
                                                       . drop 4 $ unpack program
                                                  m    = cast $ length program
                                                  a    = readInt . pack $ head code
                                                  b    = readLast $ tail code
                                                  x    = if mult then (a * b) else 0
                                               in (x +) . process' mult $ substr 1 m program
                                         else let m = cast $ length program
                                               in process' mult $ substr 1 m program

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let program = joinBy "" $ lines content
                                     silver  = show $ process program
                                     gold    = show $ process' True program
                                     in putStrLn $ "\tSilver: " ++ silver
                                              ++ "\n\tGold:   "   ++ gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day03.txt"
