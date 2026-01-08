-- result format
inductive BisectionResult where
  | success (root : Float) (iterations : Nat)
  | invalidBounds (reason : String)
  | maxIterationsReached (bestApprox : Float) (iterations : Nat)

-- iterations and max tolerance
structure BisectionConfig where
  tolerance : Float := 1e-10
  maxIterations : Nat := 1000

def oppositeSigns (x y : Float) : Bool :=
  (x > 0.0 && y < 0.0) || (x < 0.0 && y > 0.0)

def bisection (f : Float → Float) (a b : Float) (config : BisectionConfig := {}) : BisectionResult :=
  if a >= b then
    BisectionResult.invalidBounds "left bound must be less than right bound"
  else if ¬(oppositeSigns (f a) (f b)) then
    BisectionResult.invalidBounds "function must have opposite signs at bounds"
  else
    let rec loop (left right : Float) (iter : Nat) : BisectionResult :=
      if iter >= config.maxIterations then
        BisectionResult.maxIterationsReached ((left + right) / 2.0) iter
      else
        let mid := (left + right) / 2.0
        let fmid := f mid
        if fmid.abs < config.tolerance || right - left < config.tolerance then
          BisectionResult.success mid (iter + 1)
        else if oppositeSigns (f left) fmid then
          loop left mid (iter + 1)
        else
          loop mid right (iter + 1)
    loop a b 0

-- wrapper (opt)
def findRoot (f : Float → Float) (a b : Float) : BisectionResult :=
  bisection f a b

-- display
instance : Repr BisectionResult where
  reprPrec r _ := match r with
    | .success root iter => s!"Root: {root} ({iter} iterations)"
    | .invalidBounds reason => s!"Invalid: {reason}"
    | .maxIterationsReached approx iter => s!"Max iterations: {approx} ({iter} iterations)"


-- tests
def testFunction1 (x : Float) : Float := x * x - 2.0
def testFunction2 (x : Float) : Float := x * x * x - x - 1.0
def testFunction3 (x : Float) : Float := Float.sin x
def testFunction4 (x : Float) : Float := Float.sin x - Float.cos x

#eval! findRoot testFunction1 1.0 2.0  -- √2
#eval! findRoot testFunction2 1.0 2.0  -- x³ - x - 1 = 0
#eval! findRoot testFunction3 3.0 4.0  -- π
#eval! findRoot testFunction4 3.0 4.0
