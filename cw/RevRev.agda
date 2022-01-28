{-# OPTIONS --safe #-}
module RevRev where

open import Relation.Binary.PropositionalEquality
open Relation.Binary.PropositionalEquality.≡-Reasoning
open import Data.List
open import Data.List.Properties
-- open import Rev


-- What you've just imported:

-- With this definition, Agda deduces better
rev : ∀ {ℓ} {A : Set ℓ} → List A → List A
rev [] = []
rev (x ∷ xs) = rev xs ++ x ∷ []

++[] : ∀ {ℓ} {A : Set ℓ} (a : List A) → a ++ [] ≡ a
++[] [] = refl
++[] (x ∷ xs) = cong (λ x₁ → x ∷ x₁) (++[] xs)

++assoc : ∀ {ℓ} {A : Set ℓ} (a b c : List A) → a ++ b ++ c ≡ (a ++ b) ++ c
++assoc [] ys zs = refl
++assoc (x ∷ xs) ys zs = cong (λ x₁ → x ∷ x₁) (++assoc xs ys zs)

rev-distrib : ∀ {ℓ} {A : Set ℓ} (a b : List A) → rev (a ++ b) ≡ rev b ++ rev a
rev-distrib [] ys = sym (++[] (rev ys))
rev-distrib (x ∷ xs) ys = begin
        rev (xs ++ ys) ++ x ∷ []
    ≡⟨ cong ( λ l → l ++ x ∷ [] ) (rev-distrib xs ys) ⟩
        (rev ys ++ rev xs) ++ x ∷ []
    ≡⟨ sym (++assoc (rev ys) (rev xs) (x ∷ [])) ⟩
        refl

revrevid : ∀ {ℓ} {A : Set ℓ} (a : List A) → rev (rev a) ≡ a
revrevid [] = refl
revrevid (x ∷ xs) = begin
    rev (rev xs ++ x ∷ [])
    ≡⟨ rev-distrib (rev xs) (x ∷ []) ⟩
    x ∷ rev (rev xs)
    ≡⟨ cong (λ x₁ → x ∷ x₁) (revrevid xs) ⟩ refl

revconcat : ∀ {ℓ} {A : Set ℓ} (a : List A) → (b : List A) → rev (a ++ b) ≡ rev b ++ rev a
revconcat [] lt rewrite ++-identityʳ (rev lt) = refl
revconcat (x ∷ xs) lt rewrite revconcat xs lt
                            | ++-assoc (rev lt) (rev xs) (x ∷ [])= refl
