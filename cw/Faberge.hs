{-# LANGUAGE BangPatterns #-}

module Faberge where

-- reference: https://www.geeksforgeeks.org/eggs-dropping-puzzle-binomial-coefficient-and-binary-search-solution/

-- heigth :: Integer -> Integer -> Integer
-- heigth n m = l ! (n,m)
--   where
--     l = array ((0, 0), (n, m)) [((i, j), h i j) | i <- [0 .. n], j <- [0 .. m]]
--     h 0 m = 0
--     h n 0 = 0
--     h n m = l ! (n - 1, m - 1) + l ! (n, m - 1) + 1


-- height :: Integer -> Integer -> Integer
-- height n m = sum [chooose m x | x <- [1..n]]

heigth :: Integer -> Integer -> Integer
heigth n m = sumChoose m n

sumChoose :: Integer -> Integer -> Integer
sumChoose n k = go 0 0 1
  where
    fn = factorial n 1
      where
        factorial 0 !acc = acc
        factorial !nn !acc = factorial (nn -1) (acc * nn)
    go x acc last | x < k = go (x + 1) (acc + last * (n - x) `div` (x + 1)) (last  * (n - x) `div` (x + 1))
                  | otherwise = acc

