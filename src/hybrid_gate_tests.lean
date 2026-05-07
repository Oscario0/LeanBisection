import certified_bisection
import hybrid_gate_backend
import bisection
import RealLike.Evalf
import Mathlib.Tactic

open HybridGateBackend
open CertifiedBisection

def gatePoly (x : Float) : Float := x * x - 2.0

def gateBad (x : Float) : Float := 1.0 / x

instance : BisectionProofObligation gatePoly 1.0 2.0 where
  certified := trivial

#eval safeFindRootHybrid gatePoly 1.0 2.0

instance : BisectionProofObligation gateBad (-1.0) 1.0 where
  certified := trivial

#eval safeFindRootHybrid gateBad (-1.0) 1.0

def strictGate : CertifiedParams :=
  { samples := 400, maxJump := 1.0e5, maxAbsValue := 1.0e10 }

#eval safeFindRootHybrid gatePoly 1.0 2.0 defaultParamsFloat strictGate

#eval certifiedFindRoot gatePoly 1.0 2.0
#eval certifiedFindRoot gateBad (-1.0) 1.0

def polySqrt2Coeffs : Array Float := #[-2.0, 0.0, 1.0]
def polyBadCoeffs : Array Float := #[0.0, 1.0]

def polySqrt2Problem : FloatPolynomialProblem :=
  { coeffs := polySqrt2Coeffs, left := 1.0, right := 2.0 }

def polyNoBracketProblem : FloatPolynomialProblem :=
  { coeffs := polySqrt2Coeffs, left := 2.0, right := 3.0 }

def polyReciprocalProblem : FloatPolynomialProblem :=
  { coeffs := polyBadCoeffs, left := -1.0, right := 1.0 }

#eval safeFindPolynomialProblemHybrid? polySqrt2Problem
#eval safeFindPolynomialProblemHybrid? polyNoBracketProblem
#eval safeFindPolynomialProblemHybrid? polyReciprocalProblem

-- Executable exact-lift checks at the rational layer.
#eval polySqrt2Problem.liftedRatCoeffs?
#eval polySqrt2Problem.liftedRatBounds?

-- Theorem-facing preparation checks from the same single-source problem record.
#eval polySqrt2Problem.liftedRatCoeffs?.isSome
#eval polySqrt2Problem.liftedRatBounds?.isSome

noncomputable def polySqrt2RealFunction? : Option (ℝ → ℝ) :=
  polySqrt2Problem.realFunction?

noncomputable def polySqrt2RealBounds? : Option (ℝ × ℝ) :=
  polySqrt2Problem.liftedRealBounds?

noncomputable def polySqrt2RealProblem? : Option Bisection.RealProblem :=
  polySqrt2Problem.realProblem?

noncomputable def polySqrt2CertifiedProblem? : Option Bisection.CertifiedExecutableProblem :=
  polySqrt2Problem.certifiedExecutableProblem?

def gatePolyRL {α : Type} [RealLike α] (x : α) : α := x * x - 2

-- Execute theorem-facing Real expression through RealLike lowering.
#evalf gatePolyRL 1.0
#evalf gatePolyRL 1.5
#evalf gatePolyRL 2.0

#evalf checkContinuityRealLikeDefault gatePolyRL 1.0 2.0
#evalf safeFindRootHybridRLDefault gatePolyRL 1.0 2.0

def userPolyRL {α : Type} [RealLike α] : Array α := #[-2, 0, 1]

-- Preferred RealLike-first proof-obligation workflow using the original user input.
-- This entrypoint validates input, exports the certificate, and executes.
#eval certifiedFindPolynomialRootRLDefault (α := Float) userPolyRL 1.0 2.0
#eval certifiedFindPolynomialRootRLDefault (α := Float) userPolyRL 2.0 3.0

-- Proof-obligation workflow: user input -> backend check -> exported certificate -> bisection.
def userInputOne (x : Float) : Float := x * x - 2.0

#eval
  match verifyUserInputAndExportCertificate userInputOne 1.0 2.0 with
  | .ok cert => cert.continuityChecked
  | .error _ => false

#eval
  match verifyUserInputAndExportCertificate userInputOne 1.0 2.0 with
  | .ok cert => bisectionFromExportedCertificate userInputOne 1.0 2.0 cert
  | .error message => BisectionResult.noSignChange message

#eval
  match verifyUserInputAndExportCertificate userInputOne 2.0 3.0 with
  | .ok cert => bisectionFromExportedCertificate userInputOne 2.0 3.0 cert
  | .error message => BisectionResult.noSignChange message

-- Theorem-level certificate over the `RealLike` specialization to `ℝ`.
theorem gatePolyRLCertificate12 :
    Bisection.Certificate (Bisection.realProblemOfRealLike gatePolyRL 1 2) := by
  change Bisection.Certificate
    { f := (fun x : ℝ => x * x - 2), left := (1 : ℝ), right := (2 : ℝ) }
  exact Bisection.certificateOfContinuousNonposNonneg
    (fun x : ℝ => x * x - 2) 1 2
    (by norm_num)
    (by
      have hcont : Continuous (fun x : ℝ => x * x - 2) := by
        continuity
      simpa using hcont.continuousOn)
    (by norm_num)
    (by norm_num)

-- Uses Float endpoints at runtime, but the proof obligation lives over `ℝ`.
#check
  (Bisection.bisectionRealLikeCertified?
    gatePolyRL
    1
    2
    1.0
    2.0
    gatePolyRLCertificate12
    defaultParamsFloat : RealLike.Validation (BisectionResult Float))

-- Signature check: theorem bounds are explicitly `ℝ`, not `Float`.
#check Bisection.bisectionRealLikeCertified?

-- Noncompatible evalf input should fail in a controlled, documented way.
noncomputable def userInputOneReal (x : ℝ) : ℝ := x * x - 2

/-- error: argument type mismatch for `#evalf`: expected Float → Float, got ℝ → ℝ -/
#guard_msgs in
#evalf safeFindRootHybridRLDefault userInputOneReal 1 2
