# resilient

Persistent data structures

## Binomial Heap

```
module Prio.BinomHeap
  ( BinomTree
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
```

```
data BinomTree a = BNode a | BTree Int a [BinomTree a]
  deriving (Show, Eq)
```

```
type BinomHeap a = [BinomTree a]
```

> order :: BinomTree a -> Int

Return the order of a tree.

---

> nodeCount :: BinomTree a -> Int

Return the number of nodes in a tree.

---

> node :: a -> BinomTree a

Construct a node.

---

> tree :: a -> [BinomTree a] -> BinomTree a

Construct a tree from the given node and a list of sub-trees.

---

> tree1 :: a -> a -> BinomTree a

Shortcut for creating a tree with exactly one child node.

---

> root :: BinomTree a -> a

Return the root node.

---

> mergeTrees :: Ord a => BinomTree a -> BinomTree a -> BinomTree a

Merge two trees of the same order.

---

> insertTree :: Ord a => BinomTree a -> [BinomTree a] -> [BinomTree a]

Insert a tree into the list of trees (heap), maintaining the invariant that the orders of trees in the list are distinct and increasing.

---

> insertNode :: Ord a => a -> [BinomTree a] -> [BinomTree a]

Insert a single node into the list of trees. The same guarantees as for insertTree apply.

---

> fromList :: Ord a => [a] -> [BinomTree a]

Construct a heap from a list of node elements.

---

> findMinimum :: Ord a => [BinomTree a] -> a

Return the minimum element in the heap.

---

> takeMinimum :: Ord a => [BinomTree a] -> (a, [BinomTree a])

Remove and return the minimum element of the heap together with a new version of the heap, without the removed node.
