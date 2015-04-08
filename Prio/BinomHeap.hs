module Prio.BinomHeap
  ( BinomTree
  , BinomHeap
  , order
  , nodeCount
  , node
  , tree
  , tree1
  , root
  , mergeTrees
  , insertTree
  , insertNode
  , fromList
  , findMinimum
  , takeMinimum
  ) where

data BinomTree a = BNode a | BTree Int a [BinomTree a]
  deriving (Show, Eq)

instance Ord a => Ord (BinomTree a) where
    compare (BNode   a  ) (BNode   b  ) = compare a b
    compare (BNode   a  ) (BTree _ b _) = compare a b
    compare (BTree _ a _) (BNode   b  ) = compare a b
    compare (BTree _ a _) (BTree _ b _) = compare a b

type BinomHeap a = [BinomTree a]

-- | Return the order of a tree.
order :: BinomTree a -> Int
{-# LANGUAGE INLINE order #-}
order (BNode     _) = 0
order (BTree n _ _) = n

-- | Return the number of nodes in a tree.
nodeCount :: BinomTree a -> Int
nodeCount (BNode     _) = 1
nodeCount (BTree n _ _) = 2^n

-- | Construct a node.
node :: a -> BinomTree a
{-# LANGUAGE INLINE node #-}
node = BNode

-- | Construct a tree from the given node and a list of sub-trees.
tree :: a -> [BinomTree a] -> BinomTree a
{-# LANGUAGE INLINE tree #-}
tree x [] = node x
tree x xs = BTree n x xs
  where
    n = length xs

-- | Shortcut for creating a tree with exactly one child node.
tree1 :: a -> a -> BinomTree a
{-# LANGUAGE INLINE tree1 #-}
tree1 x y = BTree 1 x [node y]

-- | Return the root node.
root :: BinomTree a -> a
root (BNode   x   ) = x
root (BTree _ x _ ) = x

-- | Merge two trees of the same order.
mergeTrees :: Ord a => BinomTree a -> BinomTree a -> BinomTree a
mergeTrees x@(BTree n a xs) y@(BTree _ b ys)
                | a < b     = BTree (n + 1) a (y:xs)
                | otherwise = BTree (n + 1) b (x:ys)
mergeTrees x@(BNode a) y@(BNode b)
                | a < b     = BTree 1 a [y]
                | otherwise = BTree 1 b [x]

-- | Insert a tree into the list of trees (heap), maintaining the invariant
--   that the orders of trees in the list are distinct and increasing.
insertTree :: Ord a => BinomTree a -> [BinomTree a] -> [BinomTree a]
insertTree s [] = [s]
insertTree s tree@(t:ts)
    | order s < order t = s:tree
    | order s > order t = t:insertTree s ts
    | otherwise         = insertTree t' ts
  where
    t' = mergeTrees s t

-- | Insert a single node into the list of trees. The same guarantees as for
--   insertTree apply.
insertNode :: Ord a => a -> [BinomTree a] -> [BinomTree a]
{-# LANGUAGE INLINE insertNode #-}
insertNode n = insertTree (node n)

-- | Construct a heap from a list of node elements.
fromList :: Ord a => [a] -> [BinomTree a]
{-# LANGUAGE INLINE fromList #-}
fromList = foldr insertNode []

-- | Return the minimum element in the heap.
findMinimum :: Ord a => [BinomTree a] -> a
{-# LANGUAGE INLINE findMinimum #-}
findMinimum = root . minimum

minimumIndex :: Ord a => [a] -> Int
minimumIndex [] = error "Cannot find minimum element of empty list."
minimumIndex (x:xs) =
    case foldr f (x, -1, 0) xs of
      (_, -1, _) -> 0
      (_,  a, b) -> b - a
  where
    f y (m, j, i) =
        if y < m
           then (y, i, succ i)
           else (m, j, succ i)

-- | Remove and return the minimum element of the heap together with a new
--   version of the heap, without the removed node.
takeMinimum :: Ord a => [BinomTree a] -> (a, [BinomTree a])
takeMinimum [] = error "Attempt to take minimum from empty heap."
takeMinimum xs =
     case splitAt (minimumIndex xs) xs of
          (_, []) -> error "takeMinimum: Error in implementation."
          (lhs, BNode   n   :rs) -> (n, lhs ++ rs)
          (lhs, BTree _ t ts:rs) -> (t, foldr insertTree (lhs ++ rs) ts)

