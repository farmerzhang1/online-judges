{-# OPTIONS --safe #-}
module MagicIsCommutative where
-- link: https://www.codewars.com/kata/5c8f5d3a542ce10001c284c3/
open import Relation.Binary.PropositionalEquality
open Relation.Binary.PropositionalEquality.≡-Reasoning

-- import Relation.Binary.EqReasoning as EqR

record IsMagical {A : Set} (_∙_ : A → A → A) : Set where
  field
    left         : ∀ x y → (x ∙ y) ∙ y  ≡  x
    right        : ∀ x y → y ∙ (y ∙ x)  ≡  x

record IsCommuntative {A : Set} (_∙_ : A → A → A) : Set where
  field
    comm         : ∀ x y → x ∙ y  ≡ y ∙ x

open IsMagical
open IsCommuntative

magic-is-commutative : {A : Set}
  (_∙_ : A → A → A)
  → IsMagical _∙_
  → IsCommuntative _∙_
magic-is-commutative {A} _·_ record { left = left ; right = right } =
  record
  {
    comm = λ x y →
      begin x · y
      ≡⟨  ⟩
  }
