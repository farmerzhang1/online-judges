{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module CPS where
import qualified GHC.List
-- examples from https://free.cofree.io/2020/01/02/cps/
toCPS :: a -> (forall r. (a -> r) -> r)
toCPS = flip ($)

fromCPS :: (forall r. (a -> r) -> r) -> a
fromCPS = ($ id)

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

addCps :: Int -> Int -> ((Int -> r) -> r)
addCps x y k = k (add x y)

squareCps :: Int -> ((Int -> r) -> r)
squareCps x k = k (square x)

pythagorasCps :: Int -> Int -> ((Int -> r) -> r)
pythagorasCps x y k =
  squareCps x $ \x_squared ->
    squareCps y $ \y_squared ->
      addCps x_squared y_squared k

data Tree = Branch Tree Tree | Leaf Int

leafSumFused :: Tree -> Int
leafSumFused = snd . go False
  where
    go True _ = (True, 1000)
    go False (Leaf 6) = (True, 1000)
    go False (Leaf x) = (False, x)
    go False (Branch l r) =
      let (bl, resl) = go False l
          (br, resr) = go False r
       in if bl || br
            then (True, 1000)
            else (False, resl + resr)

leafSumFusedCPS :: Tree -> ((Int -> r) -> r)
leafSumFusedCPS t k = k 0

leafSumCPS' :: Tree -> (Int -> r) -> r
leafSumCPS' tree k = go tree k
  where
    go (Leaf 6) _ = k 1000
    go (Leaf x) k' = k' x
    go (Branch l r) k' =
      go l $ \vl ->
        go r $ \vr ->
          k' (vl + vr)

data BF = Var String | Not BF | And BF BF | Or BF BF

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS s f k = s (`f` k)

monoidToCPS :: Monoid a => a -> (() -> a) -> a
monoidToCPS a = (a <>) . ($ ())

monoidFromCPS :: Monoid a => ((() -> a) -> a) -> a
monoidFromCPS cps = cps (const mempty)

-- left-associative <>
sumL :: [Integer]
sumL = ([1, 2, 3] <> [4, 5, 6]) <> [7, 8, 9]

-- right-associative <>
sumR :: [Integer]
sumR =
  monoidFromCPS $
    monoidToCPS [1, 2, 3]
      `chainCPS` (\_ -> monoidToCPS [4, 5, 6])
      `chainCPS` (\_ -> monoidToCPS [7, 8, 9])

monadToCPS :: Monad m => m a -> (forall r. (a -> m r) -> m r)
monadToCPS m = (m >>=)

monadFromCPS :: Monad m => (forall r. (a -> m r) -> m r) -> m a
monadFromCPS cps = cps pure

-- left-associative >>=
resL :: [Integer]
resL = [1, 2, 3] >>= (\x -> [x + 1]) >>= (\y -> [y + 2])

-- right-associative >>=
resR :: [Integer]
resR =
  monadFromCPS $
    monadToCPS [1, 2, 3]
      `chainCPS` (\x -> monadToCPS [x + 1])
      `chainCPS` (\y -> monadToCPS [y + 2])

may :: Maybe Integer
may = Just 3 >>= (\x -> Just (x + 1))

may' :: Maybe Integer
may' = monadFromCPS $ monadToCPS (Just 3) `chainCPS` (\x -> monadToCPS $ Just (x + 1))

callCC :: ((a -> (b -> r) -> r) -> (a -> r) -> r) -> (a -> r) -> r
callCC f g = _

data E = forall a. MkE [a]

lenE :: E -> Int
lenE (MkE xs) = GHC.List.length xs

idd :: (a ~ b) => a -> b
idd x = x
