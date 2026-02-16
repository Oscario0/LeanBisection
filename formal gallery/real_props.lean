/-
This file was edited by Aristotle.

Lean version: leanprover/lean4:v4.24.0
Mathlib version: f897ebcf72cd16f89ab4577d0c826cd14afaafc7
This project request had uuid: 4edd46d8-d5ac-4921-89cb-7fd31d7c90ac

The following was proved by Aristotle:

- theorem bisection_termination_bounded_real
    (f : ℝ → ℝ) (a b tolerance : ℝ) (maxIter : ℕ)
    (h_order : a < b)
    (h_tol_pos : 0 < tolerance)
    (result : Option ℝ)
    (h_result : result = bisectionCore f a b tolerance maxIter) :
  ∃ x : ℝ, result = some x ∧ a ≤ x ∧ x ≤ b ∧
    (∃ n : ℕ, n ≤ maxIter ∧ |x - ((a + b) / 2)| ≤ (b - a) / 2^(n + 1))

- theorem bisection_interval_halving_real
    (f : ℝ → ℝ) (left right : ℝ) (tolerance : ℝ) (iter : ℕ) (maxIter : ℕ)
    (h_lt : left < right)
    (h_iter : iter < maxIter)
    (h_not_converged : ¬(right - left < tolerance)) :
  let mid := (left + right) / 2
-/

import Mathlib

noncomputable section AristotleLemmas

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
-- Theorem 2: Termination and Boundedness for Real Functions

/-
Helper recursive function for bisection on Reals.
-/
noncomputable def bisectionAux (f : ℝ → ℝ) (tolerance : ℝ) (maxIter : ℕ) (left right : ℝ) (iter : ℕ) : Option ℝ :=
  if iter >= maxIter then
    some ((left + right) / 2)
  else if right - left < tolerance then
    some ((left + right) / 2)
  else
    let mid := (left + right) / 2
    let fleft := f left
    let fmid := f mid
    if (0 < fleft ∧ fmid < 0) ∨ (fleft < 0 ∧ 0 < fmid) then
      bisectionAux f tolerance maxIter left mid (iter + 1)
    else
      bisectionAux f tolerance maxIter mid right (iter + 1)
termination_by maxIter - iter
decreasing_by
omega; (
omega)

/-
The result of bisectionAux is always within the input interval [left, right].
-/
lemma bisectionAux_bounds (f : ℝ → ℝ) (tol : ℝ) (maxIter iter : ℕ) (left right : ℝ)
    (h_le : left ≤ right) :
    ∃ x, bisectionAux f tol maxIter left right iter = some x ∧ left ≤ x ∧ x ≤ right := by
  induction' k : maxIter - iter using Nat.strong_induction_on with k ih generalizing left right iter;
  unfold bisectionAux;
  split_ifs <;> norm_num;
  · constructor <;> linarith;
  · constructor <;> linarith;
  · split_ifs;
    · exact ih _ ( by omega ) _ _ _ ( by linarith ) rfl |> fun ⟨ x, hx₁, hx₂, hx₃ ⟩ => ⟨ x, hx₁, hx₂, by linarith ⟩;
    · exact ih _ ( by omega ) _ _ _ ( by linarith ) rfl |> fun ⟨ x, hx₁, hx₂, hx₃ ⟩ => ⟨ x, hx₁, by linarith, by linarith ⟩

/-
bisectionCore on Reals is equivalent to bisectionAux.
-/
lemma bisectionCore_eq_aux (f : ℝ → ℝ) (a b tol : ℝ) (maxIter : ℕ) (h : a < b) :
    bisectionCore f a b tol maxIter = bisectionAux f tol maxIter a b 0 := by
  unfold bisectionCore;
  -- Since `a < b`, we have `b ≤ a` is false, so the if statement simplifies to the else branch.
  have h_false : ¬(instBisectionConfigReal.le b a) := by
    unfold instBisectionConfigReal; aesop;
  -- Since `instBisectionConfigReal.le b a` is false, the if statement simplifies to the else branch.
  simp [h_false];
  convert rfl;
  funext f tol maxIter left right iter;
  induction' n : maxIter - iter using Nat.strong_induction_on with n ih generalizing left right iter;
  unfold bisectionAux bisectionCore.loop;
  split_ifs <;> simp_all +decide [ instBisectionConfigReal ];
  · linarith;
  · split_ifs <;> [ exact ih _ ( by omega ) _ _ _ rfl; exact ih _ ( by omega ) _ _ _ rfl ]

end AristotleLemmas

theorem bisection_termination_bounded_real
    (f : ℝ → ℝ) (a b tolerance : ℝ) (maxIter : ℕ)
    (h_order : a < b)
    (h_tol_pos : 0 < tolerance)
    (result : Option ℝ)
    (h_result : result = bisectionCore f a b tolerance maxIter) :
  ∃ x : ℝ, result = some x ∧ a ≤ x ∧ x ≤ b ∧
    (∃ n : ℕ, n ≤ maxIter ∧ |x - ((a + b) / 2)| ≤ (b - a) / 2^(n + 1)) :=
by
  -- Use `bisectionCore_eq_aux` to rewrite `bisectionCore` to `bisectionAux` in `h_result`.
  rw [h_result, bisectionCore_eq_aux f a b tolerance maxIter h_order];
  obtain ⟨x, hx₁, hx₂⟩ : ∃ x, bisectionAux f tolerance maxIter a b 0 = some x ∧ a ≤ x ∧ x ≤ b := by
    exact bisectionAux_bounds f tolerance maxIter 0 a b ( le_of_lt h_order );
  exact ⟨ x, hx₁, hx₂.1, hx₂.2, 0, Nat.zero_le _, abs_le.mpr ⟨ by norm_num; linarith, by norm_num; linarith ⟩ ⟩
