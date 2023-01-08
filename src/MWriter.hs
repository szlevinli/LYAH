{-# LANGUAGE InstanceSigs #-}

module MWriter
  ( multWithLog,
    gcd',
    runMWriter,
  )
where

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

newtype MWriter w a = MWriter {runMWriter :: (a, w)}

instance (Monoid w) => Functor (MWriter w) where
  fmap :: (a -> b) -> MWriter w a -> MWriter w b
  fmap f (MWriter (a, w)) = MWriter (f a, w)

instance (Monoid w) => Applicative (MWriter w) where
  pure :: a -> MWriter w a
  pure a = MWriter (a, mempty)

  (<*>) :: MWriter w (a -> b) -> MWriter w a -> MWriter w b
  MWriter (f, w) <*> MWriter (a, w') = MWriter (f a, w `mappend` w')

instance (Monoid w) => Monad (MWriter w) where
  return :: a -> MWriter w a
  return = pure

  (>>=) :: MWriter w a -> (a -> MWriter w b) -> MWriter w b
  MWriter (a, w) >>= f =
    let MWriter (b, w') = f a
     in MWriter (b, w <> w')