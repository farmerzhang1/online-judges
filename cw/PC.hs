{-# LANGUAGE
  FlexibleInstances,
  UndecidableInstances,
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (\f b1 b2 -> ab $ f (ba b1) (ba b2), \f a1 a2 -> ba $ f (ab a1) (ab a2))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where -- this is type classes!!!
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf -- lazy! 我谢谢您嘞
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf 妙哉
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero = O
  successor = S

  nat a f O = a
  nat a f (S k) = nat (f k) f k -- i don't know what am I doing(
  -- iter :: a -> (a -> a) -> n -> a -- Induction
  iter a f O = a
  iter a f (S k) = iter (f a) f k

  plus O m = m
  plus (S k) m = S $ plus k m

  minus O m = zero
  minus n O = n
  minus (S k) (S m) = minus k m

  mult O m = zero
  mult (S k) m = plus m $ mult k m

  pow k O = S zero
  pow k (S m) = mult k $ pow k m

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (),
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where -- type: list of unit
  zero = []
  successor n = ():n

  nat a f [] = a
  nat a f (_:units) = nat (f units) f units -- i don't know what am I doing(
  -- iter :: a -> (a -> a) -> n -> a -- Induction
  iter a f [] = a
  iter a f (_:units) = iter (f a) f units

  plus = (++)

  minus [] m = []
  minus n [] = n
  minus (_:k) (_:m) = minus k m

  mult [] m = zero
  mult (_:k) m = plus m $ mult k m

  pow k [] = [()]
  pow k (_:m) = mult k $ pow k m


-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  zero = Scott (\a f -> a)
  successor scott = Scott (\a f -> f scott)
  nat a f n = runScott' a (\scott -> nat (f scott) f scott) n where
    runScott' :: forall a. a -> (Scott -> a) -> Scott -> a -- runScott' is a pattern matching function
    runScott' z s (Scott f) = f z s

  iter a f n = runScott' a (\scott -> iter (f a) f scott) n where
    runScott' :: forall a. a -> (Scott -> a) -> Scott -> a
    runScott' z s (Scott f) = f z s

  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero = Church (\f a -> a)
  successor (Church ff) = Church (\f a-> f (ff f a))
  nat :: a -> (Church -> a) -> Church -> a
  nat a f (Church ff) = a
  iter a f (Church ff) = a
  plus (Church f1) (Church f2) = Church (\f a -> f1 f (f2 f a))
  -- minus (Church f1) (Church f2) = Church (\f a -> f2 (\n -> runChurch (pred1 (Church n)) f2)) -- 又晕了
  mult (Church f1) (Church f2) = Church (f1. f2)

-- predcessor: https://en.wikipedia.org/wiki/Church_encoding
pred1 :: Church -> Church
pred1 (Church n) = Church (\f a -> extract (n (\g h -> h (g f)) (const a))) where
  extract k = k id

predChurch :: forall a. ((a -> a) -> a -> a) -> (a -> a) -> a -> a
predChurch n f a1 = extract (n (\g h -> h (g f)) (const a1)) where
  extract k = k id

predTest n = runChurch $ pred1 (Church n)