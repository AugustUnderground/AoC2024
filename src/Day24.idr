module Day24

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.List1
import Data.SortedMap as M
import Data.SortedSet as S

readInt : String -> Int
readInt = fromMaybe 0 . parseInteger {a=Int}

infixl 7 <^>
(<^>) : Int -> Int -> Int
(<^>) 0 0 = 0
(<^>) 1 1 = 0
(<^>) _ _ = 1

infixl 6 <|>
(<|>) : Int -> Int -> Int
(<|>) 0 0 = 0
(<|>) _ _ = 1

infixl 5 <&>
(<&>) : Int -> Int -> Int
(<&>) 1 1 = 1
(<&>) _ _ = 0

readInitialConditions : String -> (String, Int)
readInitialConditions s with (forget $ split (==':') s)
  readInitialConditions s | [wire,value] = (wire, readInt value)
  readInitialConditions s |      _       = ("", 0)

readGates : String -> ((String,String), (String,String))
readGates s with (unpack s)
  readGates s | l = 
    let a = pack $ takeWhile (/=' ') l 
        o = pack . takeWhile (/=' ') . drop 1 $ dropWhile (/=' ') l 
        b = pack . takeWhile (/=' ') . drop 1 . dropWhile (/=' ') . drop 1
                 $ dropWhile (/=' ') l 
        q = pack . drop 2 $ dropWhile (/='>') l
     in ((a,b), (o,q))

parse : List (List String)
      -> (SortedMap String Int, List ((String,String),(String,String)))
parse [ic', gs'] = (ic,gs)
  where
    ic = M.fromList $ map readInitialConditions ic'
    gs = map readGates gs'
parse     _      = (M.empty, [])

hasInput : SortedMap String Int -> String -> String -> Bool
hasInput m a b with (S.keySet m)
  hasInput m a b | s = (S.contains a s) && (S.contains b s)

compute' : String -> Int -> Int -> Int
compute' "XOR" = (<^>)
compute' "OR"  = (<|>)
compute' "AND" = (<&>)
compute' _ = const

compute : SortedMap String Int -> (String, String) -> (String, String)
        -> (String,Int)
compute wires (a,b) (o,q) with (M.lookup a wires) | (M.lookup b wires)
  compute wires (a,b) (o,q) | Just a' | Just b' = (q, compute' o a' b')
  compute wires (a,b) (o,q) | _       | _       = (q, 0)

simulate : SortedSet String -> List ((String,String), (String,String))
        -> SortedMap String Int -> SortedMap String Int 
simulate zs gates wires with (filter (uncurry (hasInput wires) . fst) gates) 
                           | ((S.intersection zs (S.keySet wires)) == zs)
  simulate zs gates wires |  _     | True = wires
  simulate zs gates wires |  []    | _    = wires
  simulate zs gates wires | gates' | _    = simulate zs gates
                                          . flip M.mergeLeft wires
                                          . M.fromList
                                          $ map (uncurry (compute wires)) gates'

toDec : List Int -> Int
toDec bits = foldl dec 0 $ zip bts [0 .. len]
  where
    len = (`subtract` 1) . cast {to=Int} $ length bits
    bts = map (cast {to=Int}) bits
    dec : Int -> (Int,Int) -> Int
    dec n (b,i) = cast {to=Int} $ (cast {to=Double} n) + (cast {to=Double} b)
                                * (pow 2.0 (cast {to=Double} i))

bitIds : Int -> (String,String,String)
bitIds b = (x,y,z)
  where
    x = ("x" ++) . padLeft 2 '0' $ show b
    y = ("y" ++) . padLeft 2 '0' $ show b
    z = ("z" ++) . padLeft 2 '0' $ show b

record LogicGates where
  constructor Gates
  xor,and,or : SortedMap (String,String) String
Show LogicGates where
  show (Gates xors ands ors) = show xors ++ "\n" ++ show ands ++ "\n" ++ show ors

gateMap : List ((String,String),(String,String)) -> LogicGates
gateMap gates = Gates xors ands ors
  where
    f : ((String,String),(String,String)) -> ((String,String), String)
    f ((a,b),(_,q)) = ((a,b),q)
    xors = M.fromList . map f $ filter (("XOR"==) . fst . snd) gates
    ands = M.fromList . map f $ filter (("AND"==) . fst . snd) gates
    ors  = M.fromList . map f $ filter (("OR"==)  . fst . snd) gates

findWire' : SortedMap (String,String) String -> String -> String -> Maybe String
findWire' gates a b with (M.lookup (a,b) gates) | (M.lookup (b,a) gates)
  _ | Just w | _       = Just w
  _ | _      | Just w  = Just w
  _ | _      | Nothing = Nothing

findWire : SortedMap (String,String) String -> Int -> Maybe String
findWire gates bit with (bitIds bit)
  findWire gates bit | (x,y,_) = findWire' gates x y

swapWires' : SortedMap (String,String) String -> String -> String
          -> SortedMap (String,String) String
swapWires' gates a b with (M.toList gates)
  swapWires' gates a b | gates' =
    let swap : ((String,String),String) -> ((String,String),String)
        swap (i,q) with (q == a) | (q == b)
          swap (i,q) | True | _    = (i,b)
          swap (i,q) | _    | True = (i,a)
          swap (i,q) | _    | _    = (i,q)
     in M.fromList $ map swap gates'

swapWires : LogicGates -> String -> String -> LogicGates
swapWires (Gates xors ands ors) a b = (Gates xors' ands' ors')
  where
    xors' = swapWires' xors a b
    ands' = swapWires' ands a b
    ors'  = swapWires' ors a b

check : LogicGates -> List String -> Int -> String -> List String
check _                         swaps  45 _     = sort swaps
check ckt@(Gates _ ands _)      swaps   0 _     with (findWire ands 0)
  _ | Just carry = check ckt swaps 1 carry
  _ | Nothing    = []
check ckt@(Gates xors ands ors) swaps bit carry with
    (findWire xors bit) | (findWire ands bit)
  check ckt@(Gates xors ands ors) swaps bit carry | Just xor | Just and = 
    let (x,y,z) = bitIds bit
     in case (findWire' xors xor carry) of
             Just c => if c /= z
                          then let swaps' = z :: c :: swaps
                                   ckt'  = swapWires ckt c z 
                                in check ckt' swaps' 0 ""
                          else case (findWire' ands xor carry
                                      >>= (\q => findWire' ors and q)) of
                                    Just carry' => check ckt swaps (bit + 1) carry'
                                    Nothing     => []
             Nothing => let swaps' = xor :: and :: swaps
                            ckt'   = swapWires ckt xor and 
                         in check ckt' swaps' 0 "" 
  check ckt@(Gates xors ands ors) swaps bit carry | _        | _        = []

process : List (List String) -> (Int,String)
process input = 
  let (ic,gs) = parse input
      zs      = S.fromList . filter (isPrefixOf "z") . nub $ map (snd . snd) gs
      silver  = toDec . map snd . filter (flip S.contains zs . fst) . M.toList
              $ simulate zs gs ic
      gold    = joinBy "," $ check (gateMap gs) [] 0 ""
   in (silver,gold)

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process . forget
                                                   . split null $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day24.txt"
