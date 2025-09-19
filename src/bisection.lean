
-- result format (sum type)
inductive BisectionResult where
  | success (root : Float) (iterations : Nat)
  | invalidBounds (reason : String)
  | maxIterationsReached (bestApprox : Float) (iterations : Nat)
  | numericalError (reason : String)

-- values for safe checks
structure BisectionConfig where
  tolerance : Float := 1e-10
  maxIterations : Nat := 1000
  minInterval : Float := 1e-15

-- check if floats have opposite signs
def oppositeSigns (x y : Float) : Bool :=
  (x > 0.0 && y < 0.0) || (x < 0.0 && y > 0.0)

-- check for infinity
def isFinite (x : Float) : Bool :=
  ¬(x.isNaN || x.isInf)

-- f (find roots), a b (left right bounds)
def bisection (f : Float → Float) (a b : Float) (config : BisectionConfig := {} ) : BisectionResult :=
  if ¬(isFinite a && isFinite b) then
    BisectionResult.invalidBounds "bounds must be finite numbers"
  else if a >= b then
    BisectionResult.invalidBounds "left bound must be less than right bound"
  else
    let fa := f a
    let fb := f b

    if ¬(isFinite fa && isFinite fb) then
      BisectionResult.numericalError "Function values at bounds are not finite"
    else if fa.abs < config.tolerance then
      BisectionResult.success a 0
    else if fb.abs < config.tolerance then
      BisectionResult.success b 0
    else if ¬(oppositeSigns fa fb) then
      BisectionResult.invalidBounds "Function must have opposite signs at bounds"
    else
      -- left right -> dataset split, iter (iterations done to finding root, not useful?)
      let rec bisectionLoop (left right : Float) (iter : Nat) : BisectionResult :=
        if iter >= config.maxIterations then
          BisectionResult.maxIterationsReached ((left + right) / 2.0) iter
        else if right - left < config.minInterval then
          BisectionResult.numericalError "interval too small for float precision" /-central limit theorem (for larger data)-/
        else
          let mid := (left + right) / 2.0

          if ¬(isFinite mid) then
            BisectionResult.numericalError "midpoint exceeded acceptable limit"
          else
            let fmid := f mid

            if ¬(isFinite fmid) then
              BisectionResult.numericalError s!"Function value at {mid} is not finite"
            else if fmid.abs < config.tolerance then -- count iterations after all infinity checks
              BisectionResult.success mid (iter + 1)
            else if right - left < config.tolerance then
              BisectionResult.success mid (iter + 1)
            else
              let fleft := f left
              if oppositeSigns fleft fmid then
                bisectionLoop left mid (iter + 1)
              else
                bisectionLoop mid right (iter + 1)

      bisectionLoop a b 0


def findRoot (f : Float → Float) (a b : Float) : BisectionResult :=
  bisection f a b

def BisectionResult.toString : BisectionResult → String
  | success root iter => s!"Root found: {root} (after {iter} iterations)"
  | invalidBounds reason => s!"Invalid bounds: {reason}"
  | maxIterationsReached approx iter => s!"Max iterations reached: best approximation {approx} (after {iter} iterations)"
  | numericalError reason => s!"Numerical error: {reason}"

-- note for zulip : recent lean; now requires repr typeclass to display eval results
-- for custom types
instance : Repr BisectionResult where
  reprPrec r _ := BisectionResult.toString r


instance : ToString BisectionResult where
  toString := BisectionResult.toString

-- Example usage and tests
def testFunction1 (x : Float) : Float := x * x - 2.0  -- Root at √2 ≈ 1.414

def testFunction2 (x : Float) : Float := x * x * x - x - 1.0  -- Root ≈ 1.324

def testFunction3 (x : Float) : Float := Float.sin x  -- Root at π ≈ 3.14159
def testFunction3 (x : Float) : Float := Float.sin x  -- Root at π ≈ 3.14159
def testFunction3 (x : Float) : Float := Float.sin x  -- Root at π ≈ 3.14159


-- Test cases
#eval! findRoot testFunction1 1.0 2.0  -- √2
#eval! findRoot testFunction2 1.0 2.0  -- ≈ 1.324 | x³ - x - 1 = 0
#eval! findRoot testFunction3 3.0 4.0  -- ≈ π
