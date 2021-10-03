{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Church where

newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }

pred1 :: Church -> Church
pred1 (Church n) = Church (\f a -> extract (n (\g h -> h (g f)) (const a))) where
  extract k = k id

pred2 :: forall a. ((a -> a) -> a -> a) -> (a -> a) -> a -> a
pred2 n = runChurch $ pred1 (Church n)

pred3 :: forall a. ((a -> a) -> a -> a) -> (a -> a) -> a -> a
pred3 n f a1 = extract (n (\g h -> h (g f)) (const a1)) where
  extract k = k id
