import Mathlib.Topology.ContinuousOn
import Mathlib.Topology.Order.IntermediateValue
import RealLike

/-- Result of a bisection search. -/
inductive BisectionResult (α : Type) where
  | success (root : α) (iterations : Nat)
  | invalidBounds (reason : String)
  | noSignChange (reason : String)
  | maxIterationsReached (bestApprox : α) (iterations : Nat)
  deriving Repr

/-- Configuration parameters for bisection. -/
structure BisectionParams (α : Type) where
  tolerance : α
  maxIterations : Nat := 1000
  deriving Repr

namespace Bisection

variable {α : Type} [RealLike α]

/--
Idealized bisection problem stated over the reals.

This is the problem we reason about mathematically. Proof obligations such as
continuity and root existence belong here rather than on the lowered Float code.
-/
structure RealProblem where
  f : ℝ → ℝ
  left : ℝ
  right : ℝ

/--
Executable bisection problem stated over a `RealLike` runtime type.

This is the problem we actually run numerically. Simple executable checks such as
endpoint ordering and sign changes are performed here.
-/
structure ExecutableProblem (α : Type) where
  f : α → α
  left : α
  right : α

/--
Proof-only admissibility certificate for the idealized real bisection problem.

This certifies that the underlying real problem is mathematically legitimate for
bisection. It does not claim that Float execution is exact; it only justifies
running the executable search against the lowered problem.
-/
structure Certificate (p : RealProblem) : Prop where
  ordered : p.left < p.right
  continuous : ContinuousOn p.f (Set.Icc p.left p.right)
  root_exists : ∃ x ∈ Set.Icc p.left p.right, p.f x = 0

/--
Generic real root-existence helper from continuity and an endpoint sign bracket.
-/
theorem existsRootOfContinuousNonposNonneg
    {f : ℝ → ℝ} {a b : ℝ}
    (hab : a < b)
    (hcont : ContinuousOn f (Set.Icc a b))
    (ha : f a ≤ 0)
    (hb : 0 ≤ f b) :
    ∃ x ∈ Set.Icc a b, f x = 0 := by
  have hzero : (0 : ℝ) ∈ Set.Icc (f a) (f b) := ⟨ha, hb⟩
  simpa using intermediate_value_Icc hab.le hcont hzero

/--
Symmetric root-existence helper when the endpoint signs are reversed.
-/
theorem existsRootOfContinuousNonnegNonpos
    {f : ℝ → ℝ} {a b : ℝ}
    (hab : a < b)
    (hcont : ContinuousOn f (Set.Icc a b))
    (ha : 0 ≤ f a)
    (hb : f b ≤ 0) :
    ∃ x ∈ Set.Icc a b, f x = 0 := by
  have hcontU : ContinuousOn f (Set.uIcc a b) := by
    simpa [Set.uIcc_of_le hab.le] using hcont
  have hzero : (0 : ℝ) ∈ Set.uIcc (f a) (f b) := Set.mem_uIcc_of_ge hb ha
  rcases intermediate_value_uIcc hcontU hzero with ⟨x, hx, hx0⟩
  have hxIcc : x ∈ Set.Icc a b := by
    simpa [Set.uIcc_of_le hab.le] using hx
  exact ⟨x, hxIcc, hx0⟩

/--
Build a bisection certificate from continuity and a nonpositive/nonnegative endpoint sign bracket.
-/
theorem certificateOfContinuousNonposNonneg
    (f : ℝ → ℝ)
    (a b : ℝ)
    (hab : a < b)
    (hcont : ContinuousOn f (Set.Icc a b))
    (ha : f a ≤ 0)
    (hb : 0 ≤ f b) :
    Certificate { f := f, left := a, right := b } := by
  refine ⟨hab, hcont, ?_⟩
  exact existsRootOfContinuousNonposNonneg hab hcont ha hb

/--
Build a bisection certificate from continuity and a nonnegative/nonpositive endpoint sign bracket.
-/
theorem certificateOfContinuousNonnegNonpos
    (f : ℝ → ℝ)
    (a b : ℝ)
    (hab : a < b)
    (hcont : ContinuousOn f (Set.Icc a b))
    (ha : 0 ≤ f a)
    (hb : f b ≤ 0) :
    Certificate { f := f, left := a, right := b } := by
  refine ⟨hab, hcont, ?_⟩
  exact existsRootOfContinuousNonnegNonpos hab hcont ha hb

/--
Executable Float problem authorized by a proof-only certificate over an idealized
real problem.

The proof is intentionally erased at runtime. The executable path still performs
its own runtime validation on the Float-side interval.
-/
structure CertifiedExecutableProblem where
  ideal : RealProblem
  exec : ExecutableProblem Float

namespace CertifiedExecutableProblem

/--
Build a certified executable problem from separate real and Float functions.

This is the preferred constructor when the theorem-facing problem is stated
directly over `ℝ`, which tends to interact more naturally with Mathlib tools
such as `fun_prop`.
-/
def ofFunctions
    (realF : ℝ → ℝ)
    (realLeft realRight : ℝ)
    (floatF : Float → Float)
    (floatLeft floatRight : Float) : CertifiedExecutableProblem where
  ideal := { f := realF, left := realLeft, right := realRight }
  exec := { f := floatF, left := floatLeft, right := floatRight }

/--
Build a certified executable problem from a single `RealLike`-polymorphic function.

The certificate talks about the `ℝ` specialization, while the executable path uses
the `Float` specialization. Float endpoints are supplied explicitly so callers stay
in control of the execution boundary.
-/
noncomputable def ofRealLike
    (f : {β : Type} → [RealLike β] → β → β)
    (realLeft realRight : ℝ)
    (floatLeft floatRight : Float) : CertifiedExecutableProblem where
  ideal := { f := fun x => f x, left := realLeft, right := realRight }
  exec := { f := fun x => f x, left := floatLeft, right := floatRight }

end CertifiedExecutableProblem

/--
Idealized real problem induced by a single `RealLike`-polymorphic function.

This helper is noncomputable because it specializes the function at `ℝ`.
-/
noncomputable def realProblemOfRealLike
    (f : {β : Type} → [RealLike β] → β → β)
    (left right : ℝ) : RealProblem where
  f := fun x => f x
  left := left
  right := right

/--
Runtime-validated interval with strictly ordered endpoints.

This records that the order check has passed at the API boundary, but it does not yet
carry a theorem-level proof. That stronger certification belongs to a later layer.
-/
structure OrderedInterval (α : Type) where
  left : α
  right : α
  deriving Repr

namespace OrderedInterval

/-- Validate that `left < right` before constructing an interval. -/
def mk? (left right : α) : RealLike.Validation (OrderedInterval α) := do
  let _ ← RealLike.require (RealLike.lt left right)
    "left bound must be less than right bound"
  pure { left, right }

end OrderedInterval

/--
Runtime-validated interval whose endpoint values have opposite signs.

Like `OrderedInterval`, this is a checked executable object rather than a theorem-carrying
certificate.
-/
structure BracketedInterval (α : Type) where
  left : α
  right : α
  deriving Repr

/-- Midpoint of an interval. -/
def midpoint (left right : α) : α :=
  RealLike.div (RealLike.add left right) (RealLike.ofNat 2)

/-- True when the two values have opposite signs. -/
def oppositeSigns (x y : α) : Bool :=
  (RealLike.isPositive x && RealLike.isNegative y) ||
    (RealLike.isNegative x && RealLike.isPositive y)

/-- True when `x` is numerically small enough to count as zero under `params.tolerance`. -/
def isZeroApprox (params : BisectionParams α) (x : α) : Bool :=
  RealLike.lt (RealLike.abs x) params.tolerance

namespace BracketedInterval

/-- Validate that an interval is ordered and brackets a sign change for `f`. -/
def mk? (f : α → α) (left right : α) : RealLike.Validation (BracketedInterval α) := do
  let ordered ← OrderedInterval.mk? left right
  let _ ← RealLike.require (oppositeSigns (f ordered.left) (f ordered.right))
    "function must have opposite signs at bounds"
  pure { left := ordered.left, right := ordered.right }

end BracketedInterval

/-- Choose the half-interval whose endpoints still have opposite signs. -/
def nextInterval (f : α → α) (left right : α) : α × α :=
  let mid := midpoint left right
  if oppositeSigns (f left) (f mid) then
    (left, mid)
  else
    (mid, right)

/-- Contractive bisection loop once initial bounds have been validated. -/
def loop (f : α → α) (params : BisectionParams α) (left right : α) (iter : Nat) :
    BisectionResult α :=
  if iter >= params.maxIterations then
    .maxIterationsReached (midpoint left right) iter
  else
    let mid := midpoint left right
    let fmid := f mid
    let width := RealLike.add right (RealLike.neg left)
    if isZeroApprox params fmid || RealLike.lt width params.tolerance then
      .success mid (iter + 1)
    else
      let next := nextInterval f left right
      loop f params next.1 next.2 (iter + 1)
termination_by params.maxIterations - iter
decreasing_by
  omega

/-- Bisection once the initial interval has already passed the executable checks. -/
def bisectionCoreChecked (f : α → α) (interval : BracketedInterval α)
    (params : BisectionParams α) : BisectionResult α :=
  loop f params interval.left interval.right 0

/--
Validated bisection entry point.

This shares the same `Validation := Except String` layer used by `#evalf` input checks.
-/
def bisectionCore? (f : α → α) (a b : α) (params : BisectionParams α) :
    RealLike.Validation (BisectionResult α) := do
  let interval ← BracketedInterval.mk? f a b
  pure <| bisectionCoreChecked f interval params

/-- Generic bisection over any `RealLike` type. -/
def bisectionCore (f : α → α) (a b : α) (params : BisectionParams α) :
    BisectionResult α :=
  if RealLike.le b a then
    .invalidBounds "left bound must be less than right bound"
  else if !(oppositeSigns (f a) (f b)) then
    .noSignChange "function must have opposite signs at bounds"
  else
    loop f params a b 0

/--
Run bisection on a certified executable problem.

The certificate is proof-only and is not inspected computationally. It certifies
the real-valued model problem, while the actual execution runs on the supplied
Float problem and still checks Float-side ordering/sign conditions at runtime.
-/
def bisectionCertified?
    (problem : CertifiedExecutableProblem)
    (_cert : Certificate problem.ideal)
    (params : BisectionParams Float) :
    RealLike.Validation (BisectionResult Float) :=
  bisectionCore? problem.exec.f problem.exec.left problem.exec.right params

/--
Unchecked wrapper for `bisectionCertified?` that returns a `BisectionResult`
directly, preserving the old executable API style.
-/
def bisectionCertified
    (problem : CertifiedExecutableProblem)
    (_cert : Certificate problem.ideal)
    (params : BisectionParams Float) :
    BisectionResult Float :=
  match bisectionCertified? problem _cert params with
  | .ok result => result
  | .error message => .invalidBounds message

/--
Certified executable bisection from a real theorem-facing function and a Float
runtime function.

The real-valued function and interval appear only in the type of the certificate.
The executable path uses only the Float data, so theorem-facing proofs can stay
strictly about `ℝ`.
-/
def bisectionCertifiedOfFunctions?
    (realF : ℝ → ℝ)
    (realLeft realRight : ℝ)
    (floatF : Float → Float)
    (floatLeft floatRight : Float)
    (_cert : Certificate { f := realF, left := realLeft, right := realRight })
    (params : BisectionParams Float) :
    RealLike.Validation (BisectionResult Float) :=
  bisectionCore? floatF floatLeft floatRight params

/--
Unchecked wrapper for `bisectionCertifiedOfFunctions?` that returns a result directly.
-/
def bisectionCertifiedOfFunctions
    (realF : ℝ → ℝ)
    (realLeft realRight : ℝ)
    (floatF : Float → Float)
    (floatLeft floatRight : Float)
    (_cert : Certificate { f := realF, left := realLeft, right := realRight })
    (params : BisectionParams Float) :
    BisectionResult Float :=
  let result :=
    bisectionCertifiedOfFunctions?
      realF realLeft realRight floatF floatLeft floatRight _cert params
  match result with
  | .ok result => result
  | .error message => .invalidBounds message

/--
Certified executable bisection for a single `RealLike`-polymorphic function.

The proof obligation talks about the `ℝ` specialization via `realProblemOfRealLike`,
while the actual numerical execution runs on the `Float` specialization. The proof is
erased at runtime.
-/
def bisectionRealLikeCertified?
    (f : {β : Type} → [RealLike β] → β → β)
    (realLeft realRight : ℝ)
    (floatLeft floatRight : Float)
    (_cert : Certificate (realProblemOfRealLike f realLeft realRight))
    (params : BisectionParams Float) :
    RealLike.Validation (BisectionResult Float) :=
  bisectionCore? (fun x => f x) floatLeft floatRight params

/--
Unchecked wrapper for `bisectionRealLikeCertified?` that returns a result directly.
-/
def bisectionRealLikeCertified
    (f : {β : Type} → [RealLike β] → β → β)
    (realLeft realRight : ℝ)
    (floatLeft floatRight : Float)
    (_cert : Certificate (realProblemOfRealLike f realLeft realRight))
    (params : BisectionParams Float) :
    BisectionResult Float :=
  match bisectionRealLikeCertified? f realLeft realRight floatLeft floatRight _cert params with
  | .ok result => result
  | .error message => .invalidBounds message

end Bisection

open Bisection

def defaultParamsFloat : BisectionParams Float :=
  { tolerance := 1e-10, maxIterations := 1000 }

noncomputable def defaultParamsReal : BisectionParams ℝ :=
  { tolerance := 1 / 1000000, maxIterations := 100 }

/-- Convenience wrapper for executable `Float` root finding. -/
def findRoot (f : Float → Float) (a b : Float) : BisectionResult Float :=
  Bisection.bisectionCore f a b defaultParamsFloat
