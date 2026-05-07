import bisection

namespace HybridGateBackend

open Bisection

class BisectionProofObligation (f : Float → Float) (a b : Float) : Prop where
  certified : True

structure HybridGateParamsRL (α : Type) where
  samples : Nat := 200
  maxJump : α
  maxAbsValue : α

def defaultHybridGateParamsRL {α : Type} [RealLike α] : HybridGateParamsRL α :=
  { samples := 200
    maxJump := 1.0e6
    maxAbsValue := 1.0e12 }

abbrev HybridGateParams := HybridGateParamsRL Float

def defaultHybridGateParams : HybridGateParams := defaultHybridGateParamsRL

def defaultParamsRL {α : Type} [RealLike α] : BisectionParams α :=
  { tolerance := 1.0e-10
    maxIterations := 1000 }

/-- RealLike-polymorphic continuity gate used for theorem-facing and evalf-compatible execution. -/
def checkContinuityRealLike
    {α : Type} [RealLike α]
    (f : α → α)
    (a b : α)
    (gate : HybridGateParamsRL α := defaultHybridGateParamsRL) : Bool :=
  if RealLike.le b a then
    false
  else if gate.samples = 0 then
    false
  else
    let step := (b - a) / RealLike.ofNat gate.samples
    let rec loop (i : Nat) (x : α) (prev : Option α) : Bool :=
      if i > gate.samples then
        true
      else
        let y := f x
        if RealLike.lt gate.maxAbsValue (RealLike.abs y) then
          false
        else
          match prev with
          | none => loop (i + 1) (x + step) (some y)
          | some py =>
              let jump := RealLike.abs (y - py)
              if RealLike.lt gate.maxJump jump then
                false
              else
                loop (i + 1) (x + step) (some y)
      termination_by gate.samples + 1 - i
    loop 0 a none

/-- RealLike-polymorphic hybrid root-finder, suitable for `#evalf` lowering from theorem-facing code. -/
def safeFindRootHybridRL
    {α : Type} [RealLike α]
    (f : α → α)
    (a b : α)
    (params : BisectionParams α := defaultParamsRL)
    (gate : HybridGateParamsRL α := defaultHybridGateParamsRL) : BisectionResult α :=
  if checkContinuityRealLike f a b gate then
    Bisection.bisectionCore f a b params
  else
    BisectionResult.noSignChange "continuity gate failed: function appears discontinuous or unstable on [a, b]"

/-- Evalf-friendly wrapper with default continuity-gate parameters. -/
def checkContinuityRealLikeDefault
    {α : Type} [RealLike α]
    (f : α → α)
    (a b : α) : Bool :=
  checkContinuityRealLike f a b defaultHybridGateParamsRL

/-- Evalf-friendly wrapper with default bisection and continuity parameters. -/
def safeFindRootHybridRLDefault
    {α : Type} [RealLike α]
    (f : α → α)
    (a b : α) : BisectionResult α :=
  safeFindRootHybridRL f a b defaultParamsRL defaultHybridGateParamsRL

/-- Horner evaluation for polynomial coefficients ordered from constant term upward. -/
def evalPolynomial {α : Type} [RealLike α] (coeffs : Array α) (x : α) : α :=
  coeffs.foldr (fun c acc => c + x * acc) 0

private def badValue (x : Float) (gate : HybridGateParams) : Bool :=
  x.isNaN || x.isInf || Float.abs x > gate.maxAbsValue


def checkContinuityNumeric
    (f : Float → Float)
    (a b : Float)
    (gate : HybridGateParams := defaultHybridGateParams) : Bool :=
  if b <= a then
    false
  else if gate.samples = 0 then
    false
  else
    let step := (b - a) / gate.samples.toFloat
    let rec loop (i : Nat) (prev : Option Float) : Bool :=
      if i > gate.samples then
        true
      else
        let x := a + step * i.toFloat
        let y := f x
        if badValue y gate then
          false
        else
          match prev with
          | none => loop (i + 1) (some y)
          | some py =>
              let jump := Float.abs (y - py)
              if jump > gate.maxJump then
                false
              else
                loop (i + 1) (some y)
      termination_by gate.samples + 1 - i
    loop 0 none

def safeFindRootHybrid
    (f : Float → Float)
    (a b : Float)
    [BisectionProofObligation f a b]
    (params : BisectionParams Float := defaultParamsFloat)
    (gate : HybridGateParams := defaultHybridGateParams) : BisectionResult Float :=
  if checkContinuityNumeric f a b gate then
    Bisection.bisectionCore f a b params
  else
    BisectionResult.noSignChange "continuity gate failed: function appears discontinuous or unstable on [a, b]"

/-- RealLike-polymorphic polynomial gateway, intended to be exercised via `#evalf`. -/
def safeFindPolynomialRootHybridRL
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α)
    (params : BisectionParams α := defaultParamsRL)
    (gate : HybridGateParamsRL α := defaultHybridGateParamsRL) : BisectionResult α :=
  safeFindRootHybridRL (fun x => evalPolynomial coeffs x) a b params gate

/-- Evalf-friendly wrapper for polynomial root finding with default parameters. -/
def safeFindPolynomialRootHybridRLDefault
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α) : BisectionResult α :=
  safeFindPolynomialRootHybridRL coeffs a b defaultParamsRL defaultHybridGateParamsRL

/-- Certificate emitted by backend validation for RealLike polynomial user input. -/
structure RealLikePolynomialProofCertificate
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α) where
  continuityChecked : Bool
  source : String := "prototype-backend-reallike"
  samplesUsed : Nat := 0

/-- Export a backend proof certificate for RealLike polynomial input. -/
def exportRealLikePolynomialProofCertificate
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α)
    (gate : HybridGateParamsRL α := defaultHybridGateParamsRL) :
    RealLikePolynomialProofCertificate coeffs a b :=
  let backendGate : HybridGateParamsRL α :=
    { samples := Nat.max gate.samples 300
      maxJump := gate.maxJump
      maxAbsValue := gate.maxAbsValue }
  { continuityChecked :=
      checkContinuityRealLike (fun x => evalPolynomial coeffs x) a b backendGate
    source := "prototype-backend-reallike"
    samplesUsed := backendGate.samples }

/-- Validate RealLike polynomial user input and return an exported certificate. -/
def verifyRealLikePolynomialInputAndExportCertificate
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α)
    (gate : HybridGateParamsRL α := defaultHybridGateParamsRL) :
    Except String (RealLikePolynomialProofCertificate coeffs a b) := do
  if RealLike.le b a then
    throw "left bound must be less than right bound"
  let cert := exportRealLikePolynomialProofCertificate coeffs a b gate
  if cert.continuityChecked then
    pure cert
  else
    throw s!"continuity proof obligation rejected by {cert.source}"

/-- Execute bisection only after consuming a RealLike polynomial proof certificate. -/
def bisectionFromRealLikePolynomialCertificate
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α)
    (cert : RealLikePolynomialProofCertificate coeffs a b)
    (params : BisectionParams α := defaultParamsRL) : BisectionResult α :=
  if cert.continuityChecked then
    Bisection.bisectionCore (fun x => evalPolynomial coeffs x) a b params
  else
    BisectionResult.noSignChange s!"continuity proof obligation rejected by {cert.source}"

/-- Preferred RealLike-first gateway: validate user input, export cert, and execute. -/
def certifiedFindPolynomialRootRL
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α)
    (params : BisectionParams α := defaultParamsRL)
    (gate : HybridGateParamsRL α := defaultHybridGateParamsRL) : BisectionResult α :=
  match verifyRealLikePolynomialInputAndExportCertificate coeffs a b gate with
  | .ok cert => bisectionFromRealLikePolynomialCertificate coeffs a b cert params
  | .error message => BisectionResult.noSignChange message

/-- Default-parameter wrapper for the preferred RealLike-first certified gateway. -/
def certifiedFindPolynomialRootRLDefault
    {α : Type} [RealLike α]
    (coeffs : Array α)
    (a b : α) : BisectionResult α :=
  certifiedFindPolynomialRootRL coeffs a b defaultParamsRL defaultHybridGateParamsRL

/--
Single-source polynomial problem entered once over a chosen scalar type.

Fast execution and theorem-facing views are then derived by lowering/raising this
single representation rather than duplicating separate problem structures.
-/
structure PolynomialProblem (α : Type) where
  coeffs : Array α
  left : α
  right : α

abbrev FloatPolynomialProblem := PolynomialProblem Float

/-- Bit-exact conversion from finite IEEE-754 `Float` to `Rat`. Returns `none` for NaN/Inf. -/
def floatToRatExact? (x : Float) : Option Rat :=
  let bits := x.toBits
  let signNeg := ((bits >>> 63) = (1 : UInt64))
  let expBits : UInt64 := (bits >>> 52) &&& (0x7ff : UInt64)
  let mantBits : UInt64 := bits &&& (0x000fffffffffffff : UInt64)
  let expNat := expBits.toNat
  let mantNat := mantBits.toNat
  if expNat = 0x7ff then
    none
  else
    let pow2 (n : Nat) : Nat := Nat.shiftLeft 1 n
    let significand : Nat :=
      if expNat = 0 then
        mantNat
      else
        pow2 52 + mantNat
    let exponent : Int :=
      if expNat = 0 then
        Int.negOfNat 1074
      else
        Int.ofNat expNat - Int.ofNat 1075
    let signedSig : Int :=
      if signNeg then
        - (Int.ofNat significand)
      else
        Int.ofNat significand
    if exponent ≥ 0 then
      some <| (signedSig : Rat) * ((pow2 (Int.toNat exponent) : Nat) : Rat)
    else
      some <| (signedSig : Rat) / ((pow2 (Int.natAbs exponent) : Nat) : Rat)

/-- Lift finite Float coefficients to exact rational coefficients. -/
def liftFloatCoefficientsToRat? (coeffs : Array Float) : Option (Array Rat) :=
  coeffs.mapM floatToRatExact?

/-- Internal integer-to-`RealLike` conversion used by exact Float lifting. -/
private def intToRealLike {α : Type} [RealLike α] (z : Int) : α :=
  match z with
  | .ofNat n => RealLike.ofNat n
  | .negSucc n => RealLike.neg (RealLike.ofNat (n + 1))

/-- Internal rational-to-`RealLike` conversion used by theorem-facing lifts. -/
private def ratToRealLike {α : Type} [RealLike α] (q : Rat) : α :=
  intToRealLike q.num / RealLike.ofNat q.den

/-- Internal exact rational-coefficient lift to a `RealLike` target type. -/
private def liftRatCoefficientsToRealLike {α : Type} [RealLike α] (coeffs : Array Rat) : Array α :=
  coeffs.map ratToRealLike

/-- Lift finite Float coefficients to any `RealLike` target via exact rationals. -/
def liftFloatCoefficientsToRealLike?
    {α : Type} [RealLike α]
    (coeffs : Array Float) : Option (Array α) :=
  (liftFloatCoefficientsToRat? coeffs).map liftRatCoefficientsToRealLike

/-- Lift finite Float interval bounds to exact rational bounds. -/
def liftFloatBoundsToRat? (left right : Float) : Option (Rat × Rat) := do
  let l ← floatToRatExact? left
  let r ← floatToRatExact? right
  pure (l, r)

/-- Internal exact rational-bound lift to a `RealLike` target type. -/
private def liftRatBoundsToRealLike
    {α : Type} [RealLike α]
    (bounds : Rat × Rat) : α × α :=
  (ratToRealLike bounds.1, ratToRealLike bounds.2)

/-- Lift finite Float interval bounds to any `RealLike` target via exact rationals. -/
def liftFloatBoundsToRealLike?
    {α : Type} [RealLike α]
    (left right : Float) : Option (α × α) :=
  (liftFloatBoundsToRat? left right).map liftRatBoundsToRealLike

/-- Lift finite Float interval bounds to exact real bounds. -/
noncomputable def liftFloatBoundsToReal? (left right : Float) : Option (ℝ × ℝ) :=
  liftFloatBoundsToRealLike? (α := ℝ) left right

namespace PolynomialProblem

variable {α β : Type}

/-- Lower a polynomial problem across scalar types with a total conversion map. -/
def lower (problem : PolynomialProblem α) (f : α → β) : PolynomialProblem β :=
  { coeffs := problem.coeffs.map f
    left := f problem.left
    right := f problem.right }

/-- Raise a polynomial problem across scalar types with a partial conversion map. -/
def raise? (problem : PolynomialProblem α) (f : α → Option β) : Option (PolynomialProblem β) := do
  let coeffs ← problem.coeffs.mapM f
  let left ← f problem.left
  let right ← f problem.right
  pure { coeffs := coeffs, left := left, right := right }

/-- Executable polynomial induced by the stored coefficients. -/
def polyFunction {γ : Type} [RealLike γ] (problem : PolynomialProblem γ) : γ → γ :=
  fun x => evalPolynomial problem.coeffs x

/-- Executable Float polynomial induced by the stored coefficients. -/
def floatFunction (problem : FloatPolynomialProblem) : Float → Float :=
  problem.polyFunction

/-- Exact rational lift of the full Float polynomial problem. -/
def liftedRatProblem? (problem : FloatPolynomialProblem) : Option (PolynomialProblem Rat) :=
  problem.raise? floatToRatExact?

/-- Exact rational lift of polynomial coefficients. -/
def liftedRatCoeffs? (problem : FloatPolynomialProblem) : Option (Array Rat) :=
  problem.liftedRatProblem?.map (fun lifted => lifted.coeffs)

/-- Exact rational lift of interval endpoints. -/
def liftedRatBounds? (problem : FloatPolynomialProblem) : Option (Rat × Rat) :=
  liftFloatBoundsToRat? problem.left problem.right

/-- Executable bisection problem derived from the single source record. -/
def execProblem {γ : Type} [RealLike γ]
    (problem : PolynomialProblem γ) : Bisection.ExecutableProblem γ :=
  { f := problem.polyFunction, left := problem.left, right := problem.right }

/-- Lift finite Float coefficients to exact Real coefficients. -/
noncomputable def liftFloatCoefficientsToReal? (coeffs : Array Float) : Option (Array ℝ) :=
  liftFloatCoefficientsToRealLike? (α := ℝ) coeffs

/-- Exact real lift of the full Float polynomial problem. -/
noncomputable def liftedRealPolynomialProblem?
    (problem : FloatPolynomialProblem) : Option (PolynomialProblem ℝ) :=
  problem.liftedRatProblem?.map
    (fun liftedRat => liftedRat.lower (fun q => ratToRealLike (α := ℝ) q))

/-- Exact real lift of polynomial coefficients. -/
noncomputable def liftedRealCoeffs? (problem : FloatPolynomialProblem) : Option (Array ℝ) :=
  problem.liftedRealPolynomialProblem?.map (fun lifted => lifted.coeffs)

/-- Exact real lift of interval endpoints. -/
noncomputable def liftedRealBounds? (problem : FloatPolynomialProblem) : Option (ℝ × ℝ) :=
  liftFloatBoundsToReal? problem.left problem.right

/-
Deprecated staging hook kept for migration experiments that need paired Float and
Real evaluators from one coefficient array.

Currently unused in the active gateway flow; prefer `execProblem` and
`realFunction?` from `PolynomialProblem`.
-/
/-
noncomputable def polynomialFunctionsOfFloatCoeffs?
    (coeffs : Array Float) : Option ((Float → Float) × (ℝ → ℝ)) := do
  let realCoeffs ← liftFloatCoefficientsToReal? coeffs
  let floatFn : Float → Float := fun x => evalPolynomial coeffs x
  let realFn : ℝ → ℝ := fun x => evalPolynomial realCoeffs x
  pure (floatFn, realFn)
-/

/-- Theorem-facing real polynomial induced from the single source record. -/
noncomputable def realFunction? (problem : FloatPolynomialProblem) : Option (ℝ → ℝ) := do
  let coeffsR ← problem.liftedRealCoeffs?
  pure (fun x => evalPolynomial coeffsR x)

/-- Real problem view induced from the single source record. -/
noncomputable def realProblem? (problem : FloatPolynomialProblem) : Option Bisection.RealProblem := do
  let lifted ← problem.liftedRealPolynomialProblem?
  pure { f := fun x => evalPolynomial lifted.coeffs x
         left := lifted.left
         right := lifted.right }

/-- Certified executable problem view induced from the single source record. -/
noncomputable def certifiedExecutableProblem?
    (problem : FloatPolynomialProblem) : Option Bisection.CertifiedExecutableProblem := do
  let ideal ← problem.realProblem?
  pure { ideal := ideal, exec := problem.execProblem }

end PolynomialProblem

/--
Gateway path from the single-source polynomial problem record.

The same record is used to derive both Float execution and theorem-facing lifted data.
-/
def safeFindPolynomialProblemHybrid?
    (problem : FloatPolynomialProblem)
    (params : BisectionParams Float := defaultParamsFloat)
    (gate : HybridGateParams := defaultHybridGateParams) : Except String (BisectionResult Float) := do
  let _ratCoeffs ←
    match problem.liftedRatCoeffs? with
    | some cs => pure cs
    | none => throw "cannot lift polynomial coefficients: NaN or Infinity encountered"
  let _ratBounds ←
    match problem.liftedRatBounds? with
    | some bounds => pure bounds
    | none => throw "cannot lift interval bounds: NaN or Infinity encountered"
  if checkContinuityNumeric problem.floatFunction problem.left problem.right gate then
    pure <| Bisection.bisectionCore problem.floatFunction problem.left problem.right params
  else
    throw "continuity gate failed: function appears discontinuous or unstable on [a, b]"

/-
Deprecated staging compatibility wrapper around `safeFindPolynomialProblemHybrid?`.

Kept temporarily for callers that still pass raw coefficient and bound tuples.
Prefer constructing `FloatPolynomialProblem` directly and calling
`safeFindPolynomialProblemHybrid?`.
-/
/-
def safeFindPolynomialRootHybrid?
    (coeffs : Array Float)
    (a b : Float)
    (params : BisectionParams Float := defaultParamsFloat)
    (gate : HybridGateParams := defaultHybridGateParams) : Except String (BisectionResult Float) :=
  let problem : FloatPolynomialProblem := { coeffs := coeffs, left := a, right := b }
  safeFindPolynomialProblemHybrid? problem params gate
-/

/-
Deprecated staging certificate retained from the early backend prototype.

Prefer `BackendProofCertificate` for exported and consumable proof obligations.
-/
/-
structure ContinuityCertificate where
  certified : Bool
  source : String := "prototype-backend"
-/

/-- Certificate emitted by backend validation for a specific user function and interval. -/
structure BackendProofCertificate (f : Float → Float) (a b : Float) where
  continuityChecked : Bool
  source : String := "prototype-backend"
  samplesUsed : Nat := 0

/-
Deprecated staging helper retained for compatibility with early prototype callers.
Prefer `exportBackendProofCertificate`.
-/
/-
def backendContinuityCertificate
    (f : Float → Float)
    (a b : Float)
    (gate : HybridGateParams := defaultHybridGateParams) : ContinuityCertificate :=
  let backendGate : HybridGateParams :=
    { samples := Nat.max gate.samples 300
      maxJump := gate.maxJump
      maxAbsValue := gate.maxAbsValue }
  { certified := checkContinuityNumeric f a b backendGate
    source := "prototype-backend" }
-/

/-- Export a backend proof certificate after continuity screening. -/
def exportBackendProofCertificate
    (f : Float → Float)
    (a b : Float)
    (gate : HybridGateParams := defaultHybridGateParams) : BackendProofCertificate f a b :=
  let backendGate : HybridGateParams :=
    { samples := Nat.max gate.samples 300
      maxJump := gate.maxJump
      maxAbsValue := gate.maxAbsValue }
  { continuityChecked := checkContinuityNumeric f a b backendGate
    source := "prototype-backend"
    samplesUsed := backendGate.samples }

/-- Validate user input and return an exported certificate when backend checks pass. -/
def verifyUserInputAndExportCertificate
    (f : Float → Float)
    (a b : Float)
    (gate : HybridGateParams := defaultHybridGateParams) : Except String (BackendProofCertificate f a b) := do
  if b <= a then
    throw "left bound must be less than right bound"
  let cert := exportBackendProofCertificate f a b gate
  if cert.continuityChecked then
    pure cert
  else
    throw s!"continuity proof obligation rejected by {cert.source}"

/-- Execute bisection only after consuming a backend-exported proof certificate. -/
def bisectionFromExportedCertificate
    (f : Float → Float)
    (a b : Float)
    (cert : BackendProofCertificate f a b)
    (params : BisectionParams Float := defaultParamsFloat) : BisectionResult Float :=
  if cert.continuityChecked then
    Bisection.bisectionCore f a b params
  else
    BisectionResult.noSignChange s!"continuity proof obligation rejected by {cert.source}"

end HybridGateBackend
