{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Church where

newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
-- please see the question I posed in
-- https://stackoverflow.com/questions/69426296/church-numerals-rigid-type-and-infinite-type

pred1 :: Church -> Church
pred1 (Church n) = Church (\f a -> extract (n (\g h -> h (g f)) (const a))) where
  extract k = k id

pred2 :: (forall a. (a -> a) -> a -> a) -> (b -> b) -> b -> b
pred2 n = runChurch $ pred1 (Church n)

pred3 :: (forall a. (a -> a) -> a -> a) -> (b -> b) -> b -> b
pred3 n f b = extract (n (\g h -> h (g f)) (const b)) where
  extract k = k id

-- test1 :: Show s => s
-- test1 = "asdasd"

test :: Num a => a
test = 42 -- 42 can be any instances of Num
