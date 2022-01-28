{-# OPTIONS --safe #-}
module Proof where

open import Relation.Binary.PropositionalEquality
open Relation.Binary.PropositionalEquality.≡-Reasoning
open import Data.Nat
open import Data.Nat.Properties

ss : (a b : ℕ) → a + suc b ≡ suc (a + b)
ss zero b = refl
ss (suc a) b = cong suc (ss a b)

sss : (a : ℕ) → suc a + suc a ≡ suc (suc (a + a))
sss a = cong suc (ss a a)

pred≡ : (a b : ℕ) → a ≡ b → pred a ≡ pred b
pred≡ (suc a) (suc b) refl = refl
pred≡ zero zero _ = refl

sseq2eq : (a b : ℕ) → suc (suc a) ≡ suc (suc b) → a ≡ b
sseq2eq a b refl = refl

invert : (a b : ℕ) → a + a ≡ b + b → a ≡ b
invert zero zero refl = refl
invert (suc a) (suc b) what = cong suc
    (invert a b (sseq2eq (a + a) (b + b) (ss≡ss what)))
    where
    ss≡ss : suc a + suc a ≡ suc b + suc b
        → suc (suc (a + a)) ≡ suc (suc (b + b))
    ss≡ss eq = begin
        suc (suc (a + a))
        ≡⟨ sym (sss a) ⟩
        suc (a + suc a)
        ≡⟨ eq ⟩
        suc (b + suc b)
        ≡⟨ sss b ⟩ refl

invert′ : (a b : ℕ) → a + a ≡ b + b → a ≡ b
invert′ zero zero _ = refl
invert′ (suc a) (suc b) eq rewrite +-comm a (suc a) | +-comm b (suc b) = cong suc (invert′ a b (cong pred (cong pred eq)))
