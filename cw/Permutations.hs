module Permutations (permutations) where

-- https://www.codewars.com/kata/5254ca2719453dcc0b00027d/train/haskell
import Control.Applicative

import Data.List hiding (permutations)
permutations :: String -> [String]
permutations s = permutationsSomeIdentical $ stat s

-- 递归！！啊啊啊
-- 参考 https://rosettacode.org/wiki/Permutations_with_some_identical_elements#Haskell
permShort :: String -> [String]
permShort "" = [""]
permShort xs = [x : y | x <- nub xs, y <- permShort $ delete x xs]

ppp :: Eq a => [a] -> [[a]]
ppp [] = [[]]
ppp xs = [ x : y | x <- xs, y <- ppp $ delete x xs]

{-
Find the largest index k such that a[k] < a[k + 1]. If no such index exists, the permutation is the last permutation.
Find the largest index l greater than k such that a[k] < a[l].
Swap the value of a[k] with that of a[l].
Reverse the sequence from a[k + 1] up to and including the final element a[n].
上面的这个是维基百科的imperative算法
下面是stackoverflow的一个小回答
https://stackoverflow.com/questions/46785290/permutation-implementation-in-haskell
-}

permutations' :: String -> [String]
permutations' xs0 = xs0 : perms xs0 []
  where
    perms [] _ = []
    perms (t : ts) is = foldr interleave (perms ts (t : is)) (permutations' is)
      where
        interleave xs r = let (_, zs) = interleave' id xs r in zs
        interleave' _ [] r = (ts, r)
        interleave' f (y : ys) r =
          let (us, zs) = interleave' (f . (y :)) ys r
           in (y : us, f (t : y : us) : zs)

stat :: Ord a => [a] -> [(a, Int)]
stat s = map (\s -> (head s, length s)) $ (group . sort) s

permutationsSomeIdentical :: [(a, Int)] -> [[a]]
permutationsSomeIdentical [] = [[]]
permutationsSomeIdentical xs =
  [ x : ys
    | (x, xs_) <- select xs,
      ys <- permutationsSomeIdentical xs_
  ]

select :: [(a, Int)] -> [(a, [(a, Int)])]
select [] = []
select ((x, n) : xs) =
  (x, xs_) :
    [ (y, (x, n) : cs)
      | (y, cs) <- select xs
    ]
  where
    xs_
      | 1 == n = xs
      | otherwise = (x, n - 1) : xs
