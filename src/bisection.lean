import Mathlib

/-- Result struct -/
inductive BisectionResult (α : Type*) where
  | success (root : α) (iterations : Nat)
  | invalidBounds (reason : String)
  | noSignChange (reason : String)
  | maxIterationsReached (bestApprox : α) (iterations : Nat)



/-- type u to floats -/
class BisectionField (α : Type*) where
  zero : α
  one : α
  two : α
  add : α → α → α
  sub : α → α → α
  div : α → α → α
  mul : α → α → α
  lt : α → α → Bool
  le : α → α → Bool
  abs : α → α
  isPositive : α → Bool
  isNegative : α → Bool
  toFloat : α → Float  -- for display

namespace BisectionField
variable {α : Type*} [BisectionField α]

def oppositeSigns (x y : α) : Bool :=
  (isPositive x && isNegative y) || (isNegative x && isPositive y)

instance : Inhabited α where default := BisectionField.zero

-- Operator instances for natural syntax
instance : Add α where add := BisectionField.add
instance : Sub α where sub := BisectionField.sub
instance : Mul α where mul := BisectionField.mul
instance : Div α where div := BisectionField.div
instance : OfNat α 0 where ofNat := BisectionField.zero
instance : OfNat α 1 where ofNat := BisectionField.one
instance : OfNat α 2 where ofNat := BisectionField.two

end BisectionField

/- ===== Instances ===== -/

instance : BisectionField Float where
  zero := 0.0
  one := 1.0
  two := 2.0
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt := (· < ·)
  le := (· ≤ ·)
  abs := Float.abs
  isPositive x := x > 0.0
  isNegative x := x < 0.0
  toFloat := id

instance : BisectionField ℚ where
  zero := 0
  one := 1
  two := 2
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt x y := decide (x < y)
  le x y := decide (x ≤ y)
  abs x := if x < 0 then -x else x
  isPositive x := decide (x > 0)
  isNegative x := decide (x < 0)
  toFloat x := x.num.toNat.toFloat / x.den.toFloat

noncomputable instance : BisectionField ℝ where
  zero := 0
  one := 1
  two := 2
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt x y := decide (x < y)
  le x y := decide (x ≤ y)
  abs := abs
  isPositive x := decide (x > 0)
  isNegative x := decide (x < 0)
  toFloat _ := 0.0  -- placeholder

/-- config  -/
structure BisectionParams (α : Type*) where
  tolerance : α
  maxIterations : Nat := 1000

def defaultParamsFloat : BisectionParams Float :=
  { tolerance := 1e-10, maxIterations := 1000 }

def defaultParamsRat : BisectionParams ℚ :=
  { tolerance := 1 / 10^10, maxIterations := 1000 }

/-core-/
def bisectionCore {α : Type*} [BisectionField α]
    (f : α → α) (a b : α) (params : BisectionParams α) : BisectionResult α :=
  if BisectionField.le b a then
    BisectionResult.invalidBounds "left bound must be less than right bound"
  else if ¬(BisectionField.oppositeSigns (f a) (f b)) then
    BisectionResult.noSignChange "function must have opposite signs at bounds"
  else
    let rec loop (left right : α) (iter : Nat) : BisectionResult α :=
      if iter >= params.maxIterations then
        let mid := BisectionField.div (BisectionField.add left right) BisectionField.two
        BisectionResult.maxIterationsReached mid iter
      else
        let mid := BisectionField.div (BisectionField.add left right) BisectionField.two
        let fmid := f mid
        let width := BisectionField.sub right left
        -- Check convergence
        if BisectionField.lt (BisectionField.abs fmid) params.tolerance ||
          BisectionField.lt width params.tolerance then
          BisectionResult.success mid (iter + 1)
        else if BisectionField.oppositeSigns (f left) fmid then
          loop left mid (iter + 1)
        else
          loop mid right (iter + 1)
      termination_by params.maxIterations - iter
    loop a b 0

/--  wrapper for floats with default params -/
def findRoot (f : Float → Float) (a b : Float) : BisectionResult Float :=
  bisectionCore f a b defaultParamsFloat

/-- wrapper for rats with default params -/
def findRootRat (f : ℚ → ℚ) (a b : ℚ) : BisectionResult ℚ :=
  bisectionCore f a b defaultParamsRat

instance {α : Type*} [BisectionField α] : Repr (BisectionResult α) where
  reprPrec r _ := match r with
    | .success root iter =>
        s!"✓ Root: {BisectionField.toFloat root} ({iter} iterations)"
    | .invalidBounds reason =>
        s!"✗ Invalid bounds: {reason}"
    | .noSignChange reason =>
        s!"✗ No sign change: {reason}"
    | .maxIterationsReached approx iter =>
        s!"⚠ Max iterations: best approx = {BisectionField.toFloat approx} ({iter} iterations)"
