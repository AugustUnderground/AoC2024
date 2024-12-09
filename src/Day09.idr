module Day09

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.List1

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

maximum : List Int -> Int
maximum = fromMaybe 0 . head' . reverse . sort

mapDisk : Int -> List (Int, List Int) -> List Int -> List Int
              -> (List (Int, List Int), List Int)
mapDisk _ disk          free []             = (disk, reverse free)
mapDisk i []            free (f :: fs)      = mapDisk (i + 1) m free fs 
  where
    m = [(i, [0 .. f - 1])]
mapDisk i disk@(d :: _) free (r :: f :: fs) = mapDisk (i + 1) (m :: disk) e fs
  where
    l = fromMaybe 0 . last' $ snd d
    m = (i, [l + r + 1 .. f + r + l])
    e = if r > 0 then [l + r .. l + 1] ++ free else free
mapDisk _   _           _    _              = ([],[])

rearrange : List (Int, List Int) -> List Int -> List (Int, List Int)
rearrange disk                  []   = disk
rearrange ((i, blocks) :: disk) free with
    (takeWhile (<(maximum blocks)) . take (length blocks) $ free)
  rearrange ((i, blocks) :: disk) free | blocks' =
    if length blocks' == length blocks
       then let free' = (drop (length blocks) free) ++ blocks
             in (i, blocks') :: rearrange disk free'
       else let blks = (blocks' ++) . drop (length blocks') $ reverse blocks
             in (i, blks) :: rearrange disk []
rearrange _                    _    = []

checkSum : Int -> List Int -> Int
checkSum i = sum . map (*i)

rearrange' : List (Int, List Int) -> List (List Int) -> List (Int, List Int)
rearrange' []                    _       = []
rearrange' ((i, blocks) :: disk) sectors with
    (head' $ dropWhile ((<(length blocks)) . length) sectors)
  rearrange' ((i, blocks) :: disk) sectors | Just free =
      case ((take (length blocks) free) < blocks) of 
        True => let len      = length blocks
                    blocks'  = take len free
                    free'    = drop len free
                    sectors' = sort
                             $ if isNil free'
                                  then deleteBy (==) free sectors
                                  else free' :: deleteBy (==) free sectors
                 in (i, blocks') :: rearrange' disk sectors'
        False => (i, blocks) :: rearrange' disk sectors
  rearrange' ((i, blocks) :: disk) sectors | Nothing   =
    (i, blocks) :: rearrange' disk sectors

process : String -> (Int, Int)
process input = (silver,gold)
  where
    diskMap = mapDisk 0 [] [] . map (readInt . singleton) $ unpack input
    silver  = sum $ map (uncurry checkSum) . uncurry rearrange $ diskMap
    sectors = map forget . groupBy (\a,b => (<2) . abs $ a - b) . sort $ snd diskMap
    disk    = fst diskMap
    gold    = sum . map (uncurry checkSum) $ rearrange' disk sectors

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process . fromMaybe ""
                                                   . head' $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day09.txt"
