module Spiral where
import Data.List

rotate :: [[a]] -> [[a]]
rotate = map reverse . transpose

aaa :: Int -> [[Int]] -> [[Int]]
aaa n matrix = replicate (n + 2) 1 : reverse (1 : replicate (n + 1) 0) : (rotate . rotate) z where
    b = 1 : replicate (n - 1) 0
    c = replicate n 1
    z = zipWith3 (\a b c -> a : b : c) c b matrix

spiralize :: Int -> [[Int]]
spiralize 1 = [[1]]
spiralize 2 = [[1,1],[0,1]]
spiralize n = aaa (n - 2) (spiralize (n - 2))
