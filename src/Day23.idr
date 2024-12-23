module Day23

import System.File
import Data.String
import Data.Maybe
import Data.List
import Data.SortedMap as M
import Data.SortedSet as S

Node : Type
Node   = String
Graph : Type
Graph  = SortedMap Node (SortedSet Node)
Graph' : Type
Graph' = List (Node, (SortedSet Node))

mkGraph : List String -> Graph
mkGraph    []     = M.empty
mkGraph (e :: es) = M.mergeWith S.union a . M.mergeWith S.union b $ mkGraph es
  where
    a' = pack . take 2 $ unpack e
    b' = pack . drop 3 $ unpack e
    a  = M.singleton a' (S.singleton b')
    b  = M.singleton b' (S.singleton a')

triangle : Graph -> SortedSet Node -> Node -> List (List Node)
triangle graph nodes node with (M.lookup node graph)
  triangle graph nodes node | Nothing     = []
  triangle graph nodes node | Just nodes' = 
    map ((node::) . singleton) . S.toList $ S.intersection nodes nodes'

triangles : Graph -> List (SortedSet Node)
triangles graph with (M.leftMost graph)
  triangles graph | Nothing            = []
  triangles graph | Just (node, nodes) = 
    let pred : List Node -> Bool
        pred verts = (2 < length verts) && (any (isPrefixOf "t") verts)
        tris = nub. concatMap ( map S.fromList . filter pred
                              . map ((node::)) . triangle graph nodes)
             $ S.toList nodes
     in tris ++ triangles (M.delete node graph)

len : List a -> List b -> Ordering
len as bs with (length as > length bs)
             | (length as < length bs)
  len as bs | True | _    = GT
  len as bs | _    | True = LT
  len as bs | _    | _    = EQ

adj : Graph' -> (Node, SortedSet Node) -> Graph'
adj []  _   = []
adj g (v,_) = filter (S.contains v . snd) g

bkb : Graph' -> Graph' -> Graph' -> List Graph' -> List Graph'
bkb r    []    [] s = take 1 . reverse . sortBy len $ r :: s
bkb _    []     _ s = take 1 . reverse . sortBy len $ s
bkb r (p :: ps) x s = bkb r ps (p :: x) 
                    $ bkb (p :: r) (adj (p :: ps) p) (adj x p) s

clq : Graph' -> List Node
clq g = fromMaybe [] . head' . reverse . sortBy len . map (sort . map fst)
      $ bkb [] g [] []

process : List String -> (Int,String)
process input = (silver,gold)
  where
    graph  = mkGraph input
    silver = cast {to=Int} . length . nub $ triangles graph
    gold   = joinBy "," . sort . clq $ M.toList graph

public export
solve : IO ()
solve = do file <- readFile path
           case file of
                Right content => let (silver,gold) = process $ lines content
                                  in putStrLn $ "\tSilver: " ++ show silver
                                           ++ "\n\tGold:   " ++ gold
                Left  error   => putStrLn (show error)
  where
    path = "./rsc/day23.txt"
