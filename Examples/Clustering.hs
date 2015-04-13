module Examples.Clustering
  ( example1
  ) where

import Data.Function           ( on )
import Data.List               ( sortBy )
import Data.List.Split         ( splitOn )
import Data.Sequence           ( index )
import Set.UnionFind

data Edge = Edge Int -- ^ Index
                 Int -- ^ First vertex
                 Int -- ^ Second vertex
                 Int -- ^ Edge weight
  deriving (Show)

cost :: Edge -> Int
cost (Edge _ _ _ c) = c

clustering :: [Edge] -> Int -> Int -> Int
clustering edges n k =
    let sorted = sortBy (compare `on` cost) edges
     in f sorted $ disjoint [1 .. n]
  where
    f [] _ = error "..."
    f (Edge _ a b w:xs) set
        -- Check if adding the edge introduces a cycle
        | find set a == find set b = f xs set
        | otherwise =
            if k == count set
               then w
               else f xs $ union set a b

run :: String -> IO Int
run file = do
    file <- readFile file
    let (x:xs) = lines file
        nodeCount = read x
        edges = zipWith f [1..] $ xs
    return $ clustering edges nodeCount 4
  where
    f i x = let [a, b, c] = splitOn " " x
             in Edge i (read a) (read b) (read c)

example1 :: IO ()
example1 = do
    r <- run "Examples/testdata.txt"
    print r

