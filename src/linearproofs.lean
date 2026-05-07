import Mathlib.Tactic
import bisection

namespace LinearBisectionExample

/-- Real-valued linear example used for theorem-facing certification. -/
def linearRootR : ℝ → ℝ := fun x => x - (1 : ℝ)

/-- Left endpoint for the certified linear interval. -/
def linearLeft : ℝ := 0

/-- Right endpoint for the certified linear interval. -/
def linearRight : ℝ := 2

lemma linearRoot_ordered : linearLeft < linearRight := by
  unfold linearLeft linearRight
  have h : (0 : ℝ) < (1 : ℝ) := by exact zero_lt_one
  nlinarith

lemma linearRoot_continuous :
    ContinuousOn linearRootR (Set.Icc linearLeft linearRight) := by
  unfold linearRootR
  simpa using
    (show Continuous (fun x : ℝ => x - (1 : ℝ)) by
      fun_prop).continuousOn

lemma linearRoot_exists :
    ∃ x ∈ Set.Icc linearLeft linearRight, linearRootR x = 0 := by
  refine ⟨(1 : ℝ), ?_, ?_⟩
  · constructor
    · unfold linearLeft
      have h : (0 : ℝ) < (1 : ℝ) := by exact zero_lt_one
      nlinarith
    · unfold linearRight
      have h : (0 : ℝ) < (1 : ℝ) := by exact zero_lt_one
      nlinarith
  · change ((1 : ℝ) - 1 = (0 : ℝ))
    nlinarith

lemma linearCertificate :
    Bisection.Certificate { f := linearRootR, left := linearLeft, right := linearRight } := by
  exact ⟨linearRoot_ordered, linearRoot_continuous, linearRoot_exists⟩

end LinearBisectionExample
