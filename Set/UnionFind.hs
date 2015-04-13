{-# LANGUAGE RecordWildCards #-}
module Set.UnionFind
  ( Set(..)
  , union
  , find
  , disjoint
  ) where

import Data.Sequence

data Set = Set
    { count :: Int
    , ids   :: Seq Int
    , sizes :: Seq Int
    } deriving (Show)

disjoint :: [Int] -> Set
disjoint xs = Set len (Data.Sequence.fromList xs)
    $ Data.Sequence.replicate len 1
  where
    len = Prelude.length xs

find :: Set -> Int -> Int
find set@Set{..} p | p == parent = p
                  | otherwise  = find set parent
  where
    parent = index ids (pred p)

union :: Set -> Int -> Int -> Set
union set@Set{..} p q
    | i == j     = set
    | otherwise = Set (pred count) ids' sizes'
  where
    sizeIndex = index sizes . pred
    (i,  j ) = (find set p, find set q)
    (ix, jx) = (sizeIndex i, sizeIndex j)
    up = update . pred
    (ids', sizes') =
        if ix < jx
           then (up i j ids, up j (ix + jx) sizes)
           else (up j i ids, up i (ix + jx) sizes)

