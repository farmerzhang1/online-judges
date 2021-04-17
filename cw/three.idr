module Solution
%access export
%default total



data Mult3 : Nat -> Type where
  Mult_0 : Mult3 0
  Mult_SSS : (n : Nat) -> Mult3 n -> Mult3 (S (S (S n)))

data Mult3' : Nat -> Type where
  Mult_30 : Mult3' 30
  Mult_21 : Mult3' 21
  Mult_sum : (n : Nat) -> (m : Nat) -> Mult3' n -> Mult3' m -> Mult3' (n + m)
  Mult_diff : (l : Nat) -> (n : Nat) -> (m : Nat) -> Mult3' n -> Mult3' m -> l + n = m -> Mult3' l

-- only copy code below

get3_by_n_m : (n : Nat) -> (m : Nat) -> Mult3' n -> Mult3' m -> Mult3' 3
get3_by_n_m (S(S(S(Z)))) m mult3_n mult3_m = mult3_n
get3_by_n_m n m mult3_n mult3_m = let diff = if n > m then minus n m else minus m n in ?jj
                                  -- in get3_by_n_m diff m (Mult_diff diff (if n > m then m else n) (if n > m then n else m) (if n > m then mult3_m else mult3_n) (if n > m then mult3_n else mult3_m) Refl) mult3_m

mult_imp_mult' : {n : Nat} -> Mult3 n -> Mult3' n
mult_imp_mult' Mult_0 = Mult_diff 0 30 30 Mult_30 Mult_30 Refl
mult_imp_mult' (Mult_SSS (S(S(S(S(S(S(Z))))))) mult3_6) = Mult_diff 9 21 30 Mult_21 Mult_30 Refl
mult_imp_mult' (Mult_SSS k mult3_k) = ?what

-- 21 - (30 - 21) * 2 = 3 * 21 - 30 * 2
-- 30 - 21 = 9, 21 - 9 = 12, 12 - 9 = 3, 9 - 3 = 6, 6 - 3 = 3
-- 30 - 21 = 9
mult'_imp_mult : {n : Nat} -> Mult3' n -> Mult3 n
mult'_imp_mult = ?prf2