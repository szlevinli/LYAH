{-# LANGUAGE InstanceSigs #-}

module DList (toList, fromList, getDList) where

newtype DList a = DList {getDList :: [a] -> [a]}

toList :: [a] -> DList a
toList xs = DList (xs ++)

fromList :: DList a -> [a]
fromList (DList f) = f []

instance Semigroup (DList a) where
  (<>) :: DList a -> DList a -> DList a
  DList f <> DList g = DList (f . g)

instance Monoid (DList a) where
  mempty :: DList a
  mempty = DList ([] ++)