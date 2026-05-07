import hybrid_gate_backend

open HybridGateBackend

namespace CertifiedBisection

abbrev CertifiedParams := HybridGateParams

def defaultCertifiedParams : CertifiedParams := defaultHybridGateParams

def certifiedFindRoot
    (f : Float → Float)
    (a b : Float)
    (params : BisectionParams Float := defaultParamsFloat)
    (gate : CertifiedParams := defaultCertifiedParams) : BisectionResult Float :=
  match verifyUserInputAndExportCertificate f a b gate with
  | .ok cert => bisectionFromExportedCertificate f a b cert params
  | .error message => BisectionResult.noSignChange message

/-- Candidate bound pair for selecting an interval. -/
structure BoundPair (α : Type) where
  left : α
  right : α
  deriving Repr

abbrev BoundFamily (α : Type) := Array (BoundPair α)

/-- Membership predicate for bound-pair candidates in an array family. -/
def InFamily {α : Type} (family : BoundFamily α) (pair : BoundPair α) : Prop :=
  ∃ i : Fin family.size, family[i] = pair

/-- Runtime/theorem shared validity shape for a bisection interval over `ℝ`. -/
def ValidForBisectionReal (f : ℝ → ℝ) (pair : BoundPair ℝ) : Prop :=
  pair.left < pair.right ∧
    ((f pair.left ≤ 0 ∧ 0 ≤ f pair.right) ∨ (0 ≤ f pair.left ∧ f pair.right ≤ 0))

/-- Family-level certification: every candidate interval in the family is certified. -/
def FamilyCertified (f : ℝ → ℝ) (family : BoundFamily ℝ) : Prop :=
  ∀ pair, InFamily family pair →
    Bisection.Certificate { f := f, left := pair.left, right := pair.right }

/-- Width used by deterministic selection rules. -/
def boundWidth (pair : BoundPair ℝ) : ℝ :=
  pair.right - pair.left

/-
Specification-only selector and theorems.

These are intentionally declared as stubs with `sorry` for incremental development so we can
build the interface and proofs in stages without closing the goals yet.
-/
def selectBoundPair? (family : BoundFamily ℝ) : Option (BoundPair ℝ) := by
  sorry

/-- If the selector returns a pair, it must come from the input family. -/
theorem selectBoundPair_mem
    (family : BoundFamily ℝ)
    (pair : BoundPair ℝ) :
    selectBoundPair? family = some pair → InFamily family pair := by
  sorry

/-- Selector minimizes interval width among family candidates. -/
theorem selectBoundPair_minWidth
    (family : BoundFamily ℝ)
    (pair other : BoundPair ℝ) :
    selectBoundPair? family = some pair →
    InFamily family other →
    boundWidth pair ≤ boundWidth other := by
  sorry

/-- Deterministic tie-break rule: when widths tie, choose smallest left endpoint. -/
theorem selectBoundPair_tieBreakLeft
    (family : BoundFamily ℝ)
    (pair other : BoundPair ℝ) :
    selectBoundPair? family = some pair →
    InFamily family other →
    boundWidth pair = boundWidth other →
    pair.left ≤ other.left := by
  sorry

/-- Bridge theorem: selected candidate inherits a certificate from family certification. -/
theorem selectedPairCertified
    (f : ℝ → ℝ)
    (family : BoundFamily ℝ)
    (pair : BoundPair ℝ) :
    FamilyCertified f family →
    selectBoundPair? family = some pair →
    Bisection.Certificate { f := f, left := pair.left, right := pair.right } := by
  sorry

end CertifiedBisection
