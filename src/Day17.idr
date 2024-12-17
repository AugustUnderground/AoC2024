module Day17

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.List1

record Computer where
  constructor Comp
  regA, regB, regC, ptrI : Int
  out : List Int

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

pow' : Int -> Int -> Int
pow' a b = cast $ pow (cast a) (cast b)

regs : List String -> Computer
regs [a,b,c] = Comp rA rB rC 0 []
  where
    rA  = readInt . pack . drop 12 $ unpack a
    rB  = readInt . pack . drop 12 $ unpack b
    rC  = readInt . pack . drop 12 $ unpack c
regs _  = Comp 0 0 0 0 []

parse : List (List String) -> (Computer, List Int)
parse [reg',prg'] = (cmp,prg)
  where 
    cmp = regs reg'
    prg = map readInt . forget . split (==',') . pack . drop 9 . unpack
        . fromMaybe "" $ head' prg'
parse _ = (Comp 0 0 0 0 [], [])

xor' : Int -> Int -> Int
xor' 0 0 = 0
xor' 1 1 = 0
xor' _ _ = 1

xor : Int -> Int -> Int
xor 0 0 = 0
xor x 0 = x
xor 0 y = y
xor x y = b + 2 * r
  where
    b = xor' (mod x 2) (mod y 2)
    r = xor  (div x 2) (div y 2)

combo : Computer -> Int -> Int
combo (Comp a _ _ _ _) 4 = a
combo (Comp _ b _ _ _) 5 = b
combo (Comp _ _ c _ _) 6 = c
combo _                o = o

compute : Computer -> Int -> Int -> Computer
compute cmp@(Comp a b c i p) 0 o = let a' = div a . pow' 2 $ combo cmp o 
                                    in Comp a' b c (i + 2) p
compute cmp@(Comp a b c i p) 1 o = let b' = xor b o
                                    in Comp a b' c (i + 2) p
compute cmp@(Comp a b c i p) 2 o = let b' = mod (combo cmp o) 8
                                    in Comp a b' c (i + 2) p
compute cmp@(Comp 0 b c i p) 3 _ = Comp 0 b c (i + 2) p
compute cmp@(Comp a b c _ p) 3 o = Comp a b c o p
compute cmp@(Comp a b c i p) 4 _ = let b' = xor b c
                                    in Comp a b' c (i + 2) p
compute cmp@(Comp a b c i p) 5 o = let p' = snoc p $ mod (combo cmp o) 8
                                    in Comp a b c (i + 2) p'
compute cmp@(Comp a b c i p) 6 o = let b' = div a . pow' 2 $ combo cmp o 
                                    in Comp a b' c (i + 2) p
compute cmp@(Comp a b c i p) 7 o = let c' = div a . pow' 2 $ combo cmp o 
                                    in Comp a b c' (i + 2) p
compute cmp                  _ _ = cmp

run : Computer -> List (Int) -> Computer
run c@(Comp _ _ _ x _) p with (x >= (cast $ length p))
  run c@(Comp _ _ _ x _) p | True  = c
  run c@(Comp _ _ _ x _) p | False =
    let i  = fromMaybe 8 . head' $ drop (cast x) p
        o  = fromMaybe 0 . head' $ drop (cast (x + 1)) p
        c' = compute c i o
     in run c' p

testA : Computer -> List Int -> Int -> List Int
testA (Comp _ b c i p) prog a = out $ run (Comp a b c i p) prog

test : Computer -> List Int -> Int -> List Int -> List Int
test _    _    i    []        = []
test comp prog i (n :: v) = v' ++ test comp prog i v
  where
    ns = [ 8 * n + o | o <- [0 .. 7]]
    r  = reverse . take (cast i) $ reverse prog
    v' = filter ((==r) . testA comp prog) ns

test' : Computer -> List Int -> Int -> List Int -> (Int, List Int)
test' c p i v = (i + 1, test c p i v)

process : List (List String) -> (String, Int)
process input = let (comp, prog) = parse input
                    silver       = joinBy "," . map show . out $ run comp prog
                    gold         = fromMaybe 0 . head' . sort . snd
                                 . fromMaybe (0,[]) . last'
                                 $ iterateN (length prog + 1)
                                            (uncurry (test' comp prog))
                                            (1, [0])
                 in (silver, gold)

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process . forget
                                                   . split null $ lines content
                                  in putStrLn $ "\tSilver: " ++ silver
                                           ++ "\n\tGold:   " ++ show gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day17.txt"
