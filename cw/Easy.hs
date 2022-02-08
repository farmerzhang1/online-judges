module Easy where

import Data.List

solution :: String -> String -> Bool
solution = flip isSuffixOf --  drop (length s - length end) s == end

validBraces :: String -> Bool
validBraces xs = go xs ""
  where
    go :: String -> String -> Bool
    go str@(x : xs) stack@(y : ys) = case (x, y) of
      (')', '(') -> go xs ys
      (']', '[') -> go xs ys
      ('}', '{') -> go xs ys
      ('(', _) -> go xs (x : stack)
      ('[', _) -> go xs (x : stack)
      ('{', _) -> go xs (x : stack)
      (_, _) -> False
    go [] [] = True
    go ('(' : xs) [] = go xs ['(']
    go ('[' : xs) [] = go xs ['[']
    go ('{' : xs) [] = go xs ['{']
    go _ _ = False

validBraces' :: String -> Bool
validBraces' s = "" == foldr collapse [] s

collapse :: Char -> String -> String
collapse '(' (')' : xs) = xs
collapse '{' ('}' : xs) = xs
collapse '[' (']' : xs) = xs
collapse x xs = x : xs

maze =
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 2, 2, 2, 2, 2, 2, 2, 0],
    [0, 2, 0, 0, 0, 0, 0, 0, 0],
    [0, 2, 2, 2, 0, 2, 2, 2, 0],
    [0, 0, 0, 2, 0, 2, 0, 2, 0],
    [0, 2, 2, 2, 0, 2, 0, 2, 0],
    [0, 2, 0, 0, 0, 2, 0, 2, 0],
    [0, 2, 2, 2, 2, 2, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

start :: (Int, Int)
start = (7, 7)

end :: (Int, Int)
end = (1, 7)

validPosition :: Foldable t => Int -> Int -> [t a] -> Bool
validPosition x y maze = x >= 0 && y >= 0 && length maze > y && length (head maze) > x

getNode :: Int -> Int -> Maybe Integer
getNode x y = if validPosition x y maze then Just (maze !! y !! x) else Nothing

getNeighborNode :: Foldable t => Int -> Int -> t (Int, Int) -> [(Int, Int)]
getNeighborNode x y closed = filter (\(i, j) -> getNode i j == Just 2 && notElem (i, j) closed) [(x + a, y + b) | (a, b) <- [(0, -1), (0, 1), (1, 0), (-1, 0)]]

bfsSolver ::
  [(Int, Int)] ->
  [(Int, Int)] ->
  [((Int, Int), (Int, Int))] ->
  [(Int, Int)]
bfsSolver ((i, j) : xs) closed meta
  | (i, j) /= end = bfsSolver (xs ++ neighbors) ((i, j) : closed) (meta ++ [((a, b), (i, j)) | (a, b) <- neighbors])
  | otherwise = constructPath (i, j) meta []
  where
    neighbors = getNeighborNode i j closed
bfsSolver [] _ _ = []

constructPath ::
  (Eq a, Eq b) =>
  (a, b) ->
  [((a, b), (a, b))] ->
  [(a, b)] ->
  [(a, b)]
constructPath (i, j) meta route
  | not (null points) = constructPath (head points) meta (route ++ [(i, j)])
  | otherwise = route ++ [(i, j)]
  where
    points = [(x, y) | ((a, b), (x, y)) <- meta, (a, b) == (i, j)]

-- bfsSolver [start] [] []
