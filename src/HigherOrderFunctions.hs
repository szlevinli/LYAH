{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module HigherOrderFunctions where

import Data.List ()

head' :: [a] -> a
head' = foldr1 const

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

main :: IO ()
main = do
  putStrLn $ head' ["a", "b", "c"]
  putStrLn $ last' ["a", "b", "c"]