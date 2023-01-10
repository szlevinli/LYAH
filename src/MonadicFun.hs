{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Use fmap" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use concatMap" #-}

module MonadicFun
  ( mLiftM,
    mLiftM2,
    mLiftM',
    mLiftM2',
    mAp,
    mJoin,
    mFilterM,
    mFilterM',
    keepSmall,
    mFoldM,
    binSmalls,
    foldingFunction,
    solveRPN,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (foldM, liftM)
import Control.Monad.Writer (Writer, tell)
import Data.Ratio ((%))

--
-- liftM
--

{-
 Monad's `liftM` vs. Functor's `fmap`

 fmap  :: (Functor f) => (a -> b) -> f a -> f b
 liftM :: (Monad m)   => (a -> b) -> m a -> m b
-}

-- mLiftM f ma = ma >>= \a -> return $ f a
mLiftM :: (Monad m) => (a -> b) -> m a -> m b
mLiftM f ma = f <$> ma

mLiftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
mLiftM2 f ma mb = f <$> ma <*> mb

-- use `do` notation

mLiftM' :: (Monad m) => (a -> b) -> m a -> m b
mLiftM' f ma = do
  a <- ma
  return (f a)

mLiftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
mLiftM2' f ma mb = do
  a <- ma
  b <- mb
  return (f a b)

--
-- ap
--

{-
 Monad's `ap` vs. Applicative's `<*>`

 (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
 ap    :: (Monad m)       => m (a -> b) -> m a -> m b
-}

mAp :: (Monad m) => m (a -> b) -> m a -> m b
mAp mf ma = do
  f <- mf
  a <- ma
  return (f a)

--
-- join
--

{-
 Here's some food for thought: if the result of one monadic value is
 another monadic value i.e. if one monadic value is nested inside the
 other, can you flatten them to just a single normal monadic value?
 Like, if we have `Just (Just 9)`, can we make that into `Just 9`?
 It turns out that any nested monadic value can be flattened and that this is
 actually a property unique to monads.
 For this, the `join` function exists.

 join :: (Monad m) => m (m a) -> m a

> runWriter $ join (writer (writer ('a', "aaa"), "bbb"))
('a', "bbbaaa")

why??? 这里需要理解的是 `join` 函数中的 `m <- mm` 到底干了什么?
因为 `m <- mm` 是在 `do` 语法中, 因此这段代码实际执行的是 `mm >>= (\x -> x)`
现在将类型参数换成具体的类型来看看, `mm >>= (\x -> x)` 等于

> Writer (Writer ('a', "aaa"), "bbb") >>= (\x -> x)

上面的 `x` 实际上就是 `Writer ('a', "aaa")`
`(>>=)` 函数的定义

> (>>=) :: MWriter w a -> (a -> MWriter w b) -> MWriter w b
> MWriter (a, w) >>= f =
>   let MWriter (b, w') = f a
>    in MWriter (b, w <> w')

我们将 `Writer` 带入

> Writer (Writer ('a', "aaa"), "bbb") >>= (\x -> x) =
>   let Writer (b, w') = f (Writer ('a', "aaa")) -- b = 'a', w' = "aaa"
>    in Writer (b, w <> w') -- b = 'a', w = "bbb", w' = "aaa", w <> w' = "bbbaaa"
-}

mJoin :: (Monad m) => m (m a) -> m a
mJoin mm = do
  m <- mm
  m

--
-- filterM
--

mFilterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
mFilterM _ [] = return []
mFilterM f (x : xs) = do
  b <- f x
  rs <- mFilterM f xs
  if b
    then return (x : rs)
    else return rs

-- 太难看懂了
mFilterM' :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
mFilterM' p = foldr (\x -> liftA2 (\flg -> if flg then (x :) else id) (p x)) (pure [])

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
      tell ["Keeping " ++ show x]
      return True
  | otherwise = do
      tell [show x ++ " is too large, throwing it away"]
      return False

--
-- foldM
--

{-
> foldl ::              (a -> b -> a)   -> a -> [b] -> a
> foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
-}

mFoldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
mFoldM _ a [] = return a
mFoldM f a (b : xb) = do
  x <- f a b
  mFoldM f x xb

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

foldingFunction :: (Num a1, Read a1) => [a1] -> String -> Maybe [a1]
foldingFunction (x : y : ys) "*" = return ((x * y) : ys)
foldingFunction (x : y : ys) "+" = return ((x + y) : ys)
foldingFunction (x : y : ys) "-" = return ((x - y) : ys)
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

--
-- Making monads
--

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
  fmap :: (a -> b) -> Prob a -> Prob b
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
  pure :: a -> Prob a
  pure x = Prob [(x, 1 % 1)]

  (<*>) :: Prob (a -> b) -> Prob a -> Prob b
  Prob fs <*> Prob xs = Prob $ concat $ map multAll xs
    where
      multAll (x, p) = map (\(f, p') -> (f x, p * p')) fs

instance Monad Prob where
  return :: a -> Prob a
  return x = Prob [(x, 1 % 1)]

  (>>=) :: Prob a -> (a -> Prob b) -> Prob b
  m >>= f = flatten (fmap f m)
    where
      flatten :: Prob (Prob a) -> Prob a
      flatten (Prob xs) = Prob $ concat $ map multAll xs
        where
          multAll :: (Prob a, Rational) -> [(a, Rational)]
          multAll (Prob innerXs, p) = map (\(x, r) -> (x, p * r)) innerXs
