/-
This file was edited by Aristotle.

Lean version: leanprover/lean4:v4.24.0
Mathlib version: f897ebcf72cd16f89ab4577d0c826cd14afaafc7
This project request had uuid: 4edd46d8-d5ac-4921-89cb-7fd31d7c90ac

The following was proved by Aristotle:

- theorem bisection_interval_halving_real
    (f : ℝ → ℝ) (left right : ℝ) (tolerance : ℝ) (iter : ℕ) (maxIter : ℕ)
    (h_lt : left < right)
    (h_iter : iter < maxIter)
    (h_not_converged : ¬(right - left < tolerance)) :
  let mid
-/

import Mathlib

class BisectionConfig (α : Type*) where
  add         : α → α → α
  sub         : α → α → α
  div         : α → α → α
  mul         : α → α → α
  lt          : α → α → Bool
  le          : α → α → Bool
  zero        : α
  two         : α
  isPositive  : α → Bool
  isNegative  : α → Bool

instance [BisectionConfig α] : Add α where add := BisectionConfig.add

instance [BisectionConfig α] : Sub α where sub := BisectionConfig.sub

instance [BisectionConfig α] : Div α where div := BisectionConfig.div

instance [BisectionConfig α] : Mul α where mul := BisectionConfig.mul

instance : BisectionConfig Float where
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt := (· < ·)
  le := (· ≤ ·)
  zero := 0.0
  two := 2.0
  isPositive x := x > 0.0
  isNegative x := x < 0.0

instance : BisectionConfig ℚ where
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt := (· < ·)
  le := (· ≤ ·)
  zero := 0
  two := 2
  isPositive x := x > 0.0
  isNegative x := x < 0.0

noncomputable instance : BisectionConfig ℝ where
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt x y := decide (x < y)
  le x y := decide (x ≤ y)
  zero := 0
  two := 2
  isPositive x := decide (x > 0)
  isNegative x := decide (x < 0)

def bisectionCore {α : Type*} [BisectionConfig α]
    (f : α → α) (a b tolerance : α) (maxIter : ℕ) : Option α :=
  if BisectionConfig.le b a then none  -- a >= b
  else
    let rec loop (left right : α) (iter : ℕ) : Option α :=
      if iter >= maxIter then
        some ((left + right) / BisectionConfig.two)
      else if BisectionConfig.lt (right - left) tolerance then
        some ((left + right) / BisectionConfig.two)
      else
        let mid := (left + right) / BisectionConfig.two
        let fleft := f left
        let fmid := f mid
        if (BisectionConfig.isPositive fleft && BisectionConfig.isNegative fmid) ||
            (BisectionConfig.isNegative fleft && BisectionConfig.isPositive fmid) then
          loop left mid (iter + 1)
        else
          loop mid right (iter + 1)
    loop a b 0

-- Theorem 1: Interval Halving Property for Real Functions
theorem bisection_interval_halving_real
    (f : ℝ → ℝ) (left right : ℝ) (tolerance : ℝ) (iter : ℕ) (maxIter : ℕ)
    (h_lt : left < right)
    (h_iter : iter < maxIter)
    (h_not_converged : ¬(right - left < tolerance)) :
  let mid := (left + right) / 2
  let new_interval :=
    if (0 < f left ∧ f mid < 0) ∨ (f left < 0 ∧ 0 < f mid)
    then (left, mid)
    else (mid, right)
  new_interval.2 - new_interval.1 = (right - left) / 2 :=
by
  grind
