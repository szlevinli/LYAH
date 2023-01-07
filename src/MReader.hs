{-# LANGUAGE InstanceSigs #-}

module MReader
  ( boop,
    doop,
    boopDoop,
    tupled,
    tupled',
    tupled'',
    runMReader,
    mAsk,
    mAsks,
  )
where

import Data.Char (toUpper)

-- | A History with timeline type `t` and value type `a`.
newtype History t a = History {observe :: t -> a}

instance Functor (History t) where
  -- observe :: History t a -> t -> a
  -- observe hist :: t -> a
  -- f . observe hist :: t -> b
  fmap :: (a -> b) -> History t a -> History t b
  fmap f hist = History (f . observe hist)

instance Applicative (History t) where
  -- pure x = History $ const x
  pure :: a -> History t a
  pure = History . const

  -- observe fx t :: a
  -- observe ff t :: a -> b
  (<*>) :: History t (a -> b) -> History t a -> History t b
  ff <*> fx = History $ \t -> observe ff t (observe fx t)

instance Monad (History t) where
  return :: a -> History t a
  return = pure

  (>>=) :: History t a -> (a -> History t b) -> History t b
  ma >>= f = History $ \t -> observe (f (observe ma t)) t

boop :: Integer -> Integer
boop = (* 2)

doop :: Integer -> Integer
doop = (+ 10)

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return (a, b)

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> cap <*> rev

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= \x -> rev >>= \y -> return (x, y)

-- 下面想要说明的是:
-- Reader 其实是函数 (->), 可以认为是把 (->) 换了个写法, 写成 Reader
-- 函数的类型定义是: `data (->) a b`
-- Reader的类型定义是: `newtype Reader a b = Reader {runReader :: r -> a}
-- 在 Haskell 中用 `newtype` 定义的类型都是"包装类型", 即其底层仍然被包装的类型
-- 在 Reader 中即它包装的底层类型就是函数

newtype MReader r a = MReader {runMReader :: r -> a}

instance Functor (MReader r) where
  -- ra :: r -> a
  -- ra r :: a
  -- f :: a -> b
  -- f (ra r) = f a :: b
  -- 所以: \r -> f (ra r)
  -- 等于: \r -> b
  -- 也可以写成下面的样子
  -- fmap f fa = MReader $ f . runMReader fa
  -- 也可以写成下面的样子
  -- fmap f (MReader ra) = MReader $ (f. ra)
  fmap :: (a -> b) -> MReader r a -> MReader r b
  fmap f (MReader ra) = MReader $ \r -> f (ra r)

mAsk :: MReader a a
mAsk = MReader id

instance Applicative (MReader r) where
  pure :: a -> MReader r a
  pure x = MReader $ const x

  (<*>) :: MReader r (a -> b) -> MReader r a -> MReader r b
  MReader rab <*> MReader ra = MReader $ \r -> rab r $ ra r

instance Monad (MReader r) where
  return :: a -> MReader r a
  return = pure

  -- MReader ra >>= f = MReader $ \r ->
  --   let a = ra r
  --       mrb = f a
  --       rb = runMReader mrb
  --    in rb r
  (>>=) :: MReader r a -> (a -> MReader r b) -> MReader r b
  MReader ra >>= f = MReader $ \r -> runMReader (f $ ra r) r

mAsks :: (r -> a) -> MReader r a
mAsks = MReader