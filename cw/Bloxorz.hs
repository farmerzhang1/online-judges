module Bloxorz where

import Control.Applicative
import Data.Foldable
import Data.List
import qualified Data.Map as M
import Data.Maybe

-- an interesting problem
-- https://www.codewars.com/kata/5a2a597a8882f392020005e5/train/haskell

data Loc = Loc Int Int deriving (Eq, Show, Ord)

data BoxState = Upward | Horizontal | Vertical deriving (Eq, Show, Ord)

data Position = Position Loc BoxState deriving (Eq, Show, Ord)

type Maze = [[Char]]

bloxSolver :: [[Char]] -> [Char]
bloxSolver tiles = reverse . fromJust $ getST tiles >>= (\(s, t) -> bfs tiles [Position s Upward] (Position s Upward) (Position t Upward) [Position s Upward] M.empty)

getST :: Maze -> Maybe (Loc, Loc)
getST maze = case (go maze 'B' 0, go maze 'X' 0) of
  (Just x, Just y) -> Just (x, y)
  _ -> Nothing
  where
    go :: Maze -> Char -> Int -> Maybe Loc
    go (l : m) c n = (elemIndex c l >>= (Just . Loc n)) <|> go m c (n + 1)
    go [] _ _ = Nothing

avai :: Maze -> [Position] -> Position -> Bool
avai m visited p@(Position l@(Loc x y) Vertical) =
  (isJust (get m l) && get m l /= Just '0') && (get m (Loc (x + 1) y) /= Just '0' && isJust (get m (Loc (x + 1) y)))
    && p `notElem` visited
avai m visited p@(Position l@(Loc x y) Horizontal) =
  (get m l /= Just '0' && isJust (get m l)) && (get m (Loc x (y + 1)) /= Just '0' && isJust (get m (Loc x (y + 1))))
    && p `notElem` visited
avai m visited p@(Position l@(Loc x y) Upward) =
    get m l /= Just '0' && isJust (get m l) && p `notElem` visited

-- 占据两个位置的，总是考虑最左上
neighbors :: Maze -> [Position] -> Position -> [(Position, Char)]
neighbors m visited pos = filter (avai m visited . fst) (nbs pos)

(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
[] !? n = Nothing
(x : xs) !? 0 = Just x
(x : xs) !? n = xs !? (n - 1)

get :: Maze -> Loc -> Maybe Char
get m (Loc x y) = m !? x >>= (!? y)

bfs :: Maze -> [Position] -> Position -> Position -> [Position] -> M.Map Position Char -> Maybe [Char]
bfs m (top : queue) start target visited bfsTree
  | target `elem` poses = buildRoute (foldr (uncurry M.insert) bfsTree ns) target
  | otherwise =
    bfs
      m
      (queue ++ poses)
      start
      target
      (poses ++ visited)
      (foldr (uncurry M.insert) bfsTree ns)
  where
    ns = neighbors m (top : visited) top
    poses = map fst ns
    buildRoute :: M.Map Position Char -> Position -> Maybe [Char]
    buildRoute map p
      | p == start = Just []
      | otherwise = do
        c <- M.lookup p map
        rest <- buildRoute map (op (inverse c) p)
        return (c : rest)
bfs maze [] _ _ _ _ = error (show maze)

nbs :: Position -> [(Position, Char)]
nbs (Position (Loc x y) Upward) =
  [ (Position (Loc (x - 2) y) Vertical, 'U'),
    (Position (Loc (x + 1) y) Vertical, 'D'),
    (Position (Loc x (y + 1)) Horizontal, 'R'),
    (Position (Loc x (y - 2)) Horizontal, 'L')
  ]
nbs (Position (Loc x y) Horizontal) =
  [ (Position (Loc (x - 1) y) Horizontal, 'U'),
    (Position (Loc (x + 1) y) Horizontal, 'D'),
    (Position (Loc x (y + 2)) Upward, 'R'),
    (Position (Loc x (y - 1)) Upward, 'L')
  ]
nbs (Position (Loc x y) Vertical) =
  [ (Position (Loc (x - 1) y) Upward, 'U'),
    (Position (Loc (x + 2) y) Upward, 'D'),
    (Position (Loc x (y + 1)) Vertical, 'R'),
    (Position (Loc x (y - 1)) Vertical, 'L')
  ]

up :: Position -> Position
up p = fst $ head (nbs p)

down :: Position -> Position
down p = fst $ nbs p !! 1

right :: Position -> Position
right p = fst $ nbs p !! 2

left :: Position -> Position
left p = fst $ nbs p !! 3

op :: Char -> Position -> Position
op 'U' = up
op 'D' = down
op 'R' = right
op 'L' = left
op _ = id

inverse :: Char -> Char
inverse 'U' = 'D'
inverse 'D' = 'U'
inverse 'L' = 'R'
inverse 'R' = 'L'
inverse a = a
