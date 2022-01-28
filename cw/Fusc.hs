module Fusc where

-- https://www.codewars.com/kata/57040e445a726387a1001cf7/train/haskell

fusc :: Int -> Int
fusc 0 = 0
fusc 1 = 1
fusc n
  | even n = fusc (n `div` 2)
  | odd n = fusc (n `div` 2) + fusc (n `div` 2 + 1)
fusc _ = 888

fusc' :: Int -> Int
fusc' n = case n `divMod` 2 of
  (0, 0) -> 0
  (0, 1) -> 1
  (n, 0) -> fusc' n
  (n, 1) -> fusc' n + fusc' (n + 1)
  (_, _) -> 88888

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fusc'' :: Int -> Int
fusc'' n = _
