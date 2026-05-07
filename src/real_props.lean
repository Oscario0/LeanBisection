import Mathlib.Tactic
import bisection

open Bisection

noncomputable section

namespace Bisection

theorem nextInterval_width_real (f : ℝ → ℝ) (left right : ℝ) :
    let next := nextInterval f left right
    next.2 - next.1 = (right - left) / 2 := by
  dsimp [nextInterval, midpoint]
  split_ifs <;> ring

theorem loop_returns_midpoint_when_maxed
    (f : ℝ → ℝ) (params : BisectionParams ℝ) (left right : ℝ) (iter : Nat)
    (h : params.maxIterations ≤ iter) :
    loop f params left right iter = .maxIterationsReached (midpoint left right) iter := by
  simp [loop, h]

theorem bisectionCore_invalidBounds_real
    (f : ℝ → ℝ) (a b : ℝ) (params : BisectionParams ℝ) (h : b ≤ a) :
    bisectionCore f a b params = .invalidBounds "left bound must be less than right bound" := by
  simp [bisectionCore, RealLike.le, h]

end Bisection
