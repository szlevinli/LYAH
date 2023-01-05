module MWriter (multWithLog, gcd') where

import Control.Monad.Writer (Writer, tell, writer)

logNumber :: Int -> Writer [String] Int
logNumber n = writer (n, ["Got number: " ++ show n])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two."]
  return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)