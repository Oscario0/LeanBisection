import hybrid_gate_backend
import RealLike.Evalf

open HybridGateBackend

namespace LiftingTests

/-- Sample polynomial x^2 - 2 with [1, 2] bounds from Float user input. -/
def sqrt2Problem : FloatPolynomialProblem :=
  { coeffs := #[-2.0, 0.0, 1.0], left := 1.0, right := 2.0 }

def nonFinite : Float := 0.0 / 0.0

/-- Sample bad polynomial with non-finite coefficient to exercise rejection path. -/
def badCoeffProblem : FloatPolynomialProblem :=
  { coeffs := #[nonFinite, 1.0], left := 1.0, right := 2.0 }

-- Evalf-driven checks over theorem-facing lifts through generic RealLike targets.
def intervalWidthRL {α : Type} [RealLike α] (bounds : α × α) : α :=
  bounds.2 - bounds.1

def liftCoeffsRL {α : Type} [RealLike α] (coeffs : Array Float) : Option (Array α) :=
  liftFloatCoefficientsToRealLike? (α := α) coeffs

def liftBoundsRL {α : Type} [RealLike α] (left right : Float) : Option (α × α) :=
  liftFloatBoundsToRealLike? (α := α) left right

def liftBoundsWidthRL {α : Type} [RealLike α] (left right : Float) : α :=
  match liftBoundsRL (α := α) left right with
  | some bounds => intervalWidthRL bounds
  | none => 0

#evalf liftCoeffsRL sqrt2Problem.coeffs
#evalf liftBoundsRL sqrt2Problem.left sqrt2Problem.right
#evalf liftBoundsWidthRL sqrt2Problem.left sqrt2Problem.right

#evalf liftCoeffsRL badCoeffProblem.coeffs

-- Proof-style sanity checks over Float-target lowering used by `#evalf`.
example : (liftCoeffsRL (α := Float) sqrt2Problem.coeffs).isSome = true := by
  native_decide

example : (liftBoundsRL (α := Float) sqrt2Problem.left sqrt2Problem.right).isSome = true := by
  native_decide

example : (liftCoeffsRL (α := Float) badCoeffProblem.coeffs).isSome = false := by
  native_decide

end LiftingTests
