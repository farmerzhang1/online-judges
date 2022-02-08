module Sudoku where

import Data.List
import Data.Array.IArray

data Loc = Loc Int Int deriving (Eq, Show, Ord)

-- https://www.codewars.com/kata/5296bc77afba8baa690002d7/train/haskell

sudoku :: [[Int]] -> [[Int]]
sudoku s = case jjj s of
  Nothing -> s
  Just (Loc x y, n) ->
    let (xss, xs : yss) = splitAt x s
        (xs', _ : ys') = splitAt y xs
     in sudoku $ xss ++ (xs' ++ n : ys') : yss

jjj :: [[Int]] -> Maybe (Loc, Int)
jjj s = find ((== 1) . length . candidate s) cs >>= (\l -> Just (l, head (candidate s l)))
  where
    cs = [Loc x y | y <- [0 .. 8], x <- [0 .. 8]]

candidate :: [[Int]] -> Loc -> [Int]
candidate s (Loc x y) =
  if s !! x !! y == 0
    then [e | e <- [1 .. 9], e `notElem` col, e `notElem` row, e `notElem` grid]
    else []
  where
    col = map (!! y) s
    row = s !! x
    (gx, gy) = ((x `div` 3) * 3, (y `div` 3) * 3)
    grid = concatMap (take 3 . drop gy) $ (take 3 . drop gx) s

sudokuArray :: [[Int]] -> [[Int]]
sudokuArray xss = toxss $ until finished solve initial
  where
    initial :: Array (Int, Int) Int
    initial = listArray ((0,0),(8,8)) $ concat xss

    solve grid = array ((0,0),(8,8)) $ map f (assocs grid)
      where
        f ((x,y), 0) = case possible of [v] -> ((x,y), v)
                                        _   -> ((x,y), 0)
          where possible = [1..9] \\ (row ++ column ++ square)
                row    = [grid!(x', y) | x' <- [0..8]]
                column = [grid!(x, y') | y' <- [0..8]]
                square = [grid!(x', y')
                         | x' <- [x0..x0 + 2], y' <- [y0..y0 + 2]]
                  where (x0, y0) = (x `div` 3 * 3, y `div` 3 * 3)
        f ((x,y), v) = ((x,y), v)

    finished = notElem 0 . elems

    toxss grid = [[grid!(x,y) | y <- [0..8]] | x <- [0..8]]