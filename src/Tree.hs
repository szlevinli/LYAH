{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tree (Tree) where

data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElement :: (Ord a) => a -> Tree a -> Bool
treeElement _ EmptyTree = False
treeElement x (Node a left right)
  | x == a = True
  | x < a = treeElement x left
  | x > a = treeElement x right