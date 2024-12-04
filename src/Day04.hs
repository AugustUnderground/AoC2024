import qualified Data.Map as M
import           Data.Maybe (catMaybes)

main :: IO ()
main = do
  input <- inMap . lines <$> readFile "./rsc/day04.txt"
  let silver = processInput "XMAS" input
      gold   = processInput' ["MAS","SAM"] input
  putStrLn $ "Silver: " ++ show silver
  putStrLn $ "Gold:   " ++ show gold

inMap :: [String] -> M.Map (Int,Int) Char
inMap ls = M.fromList . zip idx $ foldl1 (++) ls
  where
    i   = length ls
    j   = length $ head ls
    idx = [ (i',j') | i' <- [0 .. (i - 1)], j' <- [0 .. (j - 1)] ]

processInput' :: [String] -> M.Map (Int,Int) Char -> Int
processInput' ps dct = length $ filter id i
  where
    i = [  (catMaybes [M.lookup (i'+d,j'+d) dct | d <- [-1,0,1]] `elem` ps)
            &&
            (catMaybes [M.lookup (i'+d,j'-d) dct | d <- [-1,0,1]]  `elem` ps)
          
        | (i',j') <- M.keys dct ]

processInput :: String -> M.Map (Int,Int) Char -> Int
processInput p dct = length $ filter id u 
  where
    ds  = [(x',y') | x' <- [-1,0,1], y' <- [-1,0,1]]
    u   = [ (==p) $ catMaybes [ M.lookup (i'+x*n,j'+y*n) dct | n <- [0 .. 3] ]
         | (x,y) <- ds , (i',j') <- M.keys dct ]
