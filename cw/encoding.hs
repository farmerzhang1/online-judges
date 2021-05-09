{-# LANGUAGE RankNTypes #-}

import Data.List
-- these are Scott Encoding examples from https://kseo.github.io/posts/2016-12-13-scott-encoding.html
newtype PairS a b = PairS { unpairS :: forall r. (a -> b -> r) -> r } -- forall!

pairS :: a -> b -> PairS a b
pairS a b = PairS (\p -> p a b)

fstS :: PairS a b -> a
fstS (PairS p) = p (\x _ -> x)

sndS :: PairS a b -> b
sndS (PairS p) = p (\_ y -> y)

swapS :: PairS a b -> PairS b a
swapS p = pairS (sndS p) (fstS p)

newtype NumS = NumS { unnumS :: forall r. (NumS -> r) -> r -> r }

zeroS :: NumS
zeroS = NumS (\s z -> z)

succS :: NumS -> NumS
succS n = NumS (\s z -> s n)

unnumS' :: (NumS -> r) -> r -> NumS -> r
-- pattern matching? (first argument is the non-zero case, second argument is zero, then do case-split on the third argument)
unnumS' s z (NumS f) = f s z

isZero :: NumS -> Bool
isZero = unnumS' (\_ -> False) True --(NumS f) = f (const False) True

addS :: NumS -> NumS -> NumS
addS n m = unnumS' (\s -> succS (addS s m)) m n -- holy shit!!! it's too cool

newtype ListS a =
    ListS {
        unconsS :: forall r. (a -> ListS a -> r) -> r -> r -- these are still two data constructor
    }

nilS :: ListS a
nilS = ListS (\cons nil -> nil)

consS :: a -> ListS a -> ListS a
consS x xs = ListS (\co ni -> co x xs)

unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' co ni (ListS f) = f co ni

isNullS :: ListS a -> Bool
isNullS = unconsS' (\_ _ -> False) True

mapS :: (a -> b) -> ListS a -> ListS b
mapS f =
  unconsS' (\x xs -> consS (f x) (mapS f xs))
           nilS

