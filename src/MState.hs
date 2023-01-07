{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module MState
  ( runMState,
    pop,
    push,
    pop',
    push',
    runState,
    stackManip,
    stackManip',
    stackStuff,
    randomSt,
    threeCoins,
    mkStdGen,
  )
where

import Control.Monad.State
  ( State,
    runState,
    state,
  )
import System.Random
  ( Random,
    RandomGen,
    StdGen,
    mkStdGen,
    random,
  )

newtype MState s a = MState {runMState :: s -> (a, s)}

instance Functor (MState s) where
  fmap :: (a -> b) -> MState s a -> MState s b
  fmap f (MState g) = MState $ \s ->
    let (a, s') = g s
     in (f a, s')

instance Applicative (MState s) where
  pure :: a -> MState s a
  pure x = MState (x,)
  (<*>) :: MState s (a -> b) -> MState s a -> MState s b
  MState g <*> MState h = MState $ \s ->
    let (g', s') = g s
        (a, s'') = h s'
     in (g' a, s'')

instance Monad (MState s) where
  return :: a -> MState s a
  return x = MState (x,)
  (>>=) :: MState s a -> (a -> MState s b) -> MState s b
  (MState h) >>= f = MState $ \s ->
    let (a, newState) = h s
        (MState g) = f a
     in g newState

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x : xs)

pop' :: MState Stack Int
pop' = MState $ \(x : xs) -> (x, xs)

push' :: Int -> MState Stack ()
push' x = MState $ \xs -> ((), x : xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  _ <- pop
  pop

stackManip' :: MState Stack Int
stackManip' = do
  push' 3
  _ <- pop'
  pop'

-- Pop one number off the stack and then
-- if that number `5` we just put it back onto the stack and stop
-- but if it isn't `5`, we push `3` and `8` back on.

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do
      push 3
      push 8

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)