module Foldmap where

-- https://www.codewars.com/kata/543d218022e0f307fb000173/train/haskell

import Data.Foldable (Foldable, foldMap)
import Data.Monoid
import Control.Applicative

myToList :: Foldable t => t a -> [a]
myToList = foldMap pure

-- (hint : you will have to write a custom type, with a custom Monoid instance)
myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum xs = getMin (foldMap (Minn . Just) xs)

-- (hint : there is a suitable Monoid in Data.Monoid)
myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f b xs = aa test b where
    test = foldMap (Endoo . f) xs

newtype Endoo b = Endoo {aa :: b -> b}

instance Semigroup (Endoo a) where
  (Endoo f) <> Endoo g = Endoo (f . g)
instance Monoid (Endoo a) where
    mempty = Endoo id
newtype Minn a = Minn {getMin :: Maybe a}

instance Ord a => Semigroup (Minn a) where
  -- <$> 再 <*> 算是标准做法了吧，有两个参数的时候
  (Minn a) <> (Minn b) = Minn ((min <$> a <*> b) <|> a <|> b)

instance Ord a => Monoid (Minn a) where
  mempty = Minn Nothing
