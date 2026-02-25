import bisection




/- Floats -/

def testSqrt2 (x : Float) : Float := x * x - 2.0
def testCubic (x : Float) : Float := x * x * x - x - 1.0
def testQuartic (x : Float) : Float := x^4 - 5*x^2 + 4.0

#eval! findRoot testSqrt2 1.0 2.0      -- ~ 1.414214 (34 iterations)
#eval! findRoot testCubic 1.0 2.0      -- ~ 1.324718 (34 iterations)
#eval! findRoot testQuartic 1.5 2.5    --   2.000000 (1 iteration)

-- transcendentals
def testPi (x : Float) : Float := Float.sin x
def testSinCos (x : Float) : Float := Float.sin x - Float.cos x
def testCos (x : Float) : Float := Float.cos x
def testTan (x : Float) : Float := Float.tan x - 1.0

#eval! findRoot testPi 3.0 4.0         -- ~ 3.141593 (34 iterations)
#eval! findRoot testSinCos 0.5 1.0     -- ~ 0.785398 (34 iterations)
#eval! findRoot testCos 1.0 2.0        -- ~ 1.570796 (34 iterations)
#eval! findRoot testTan 0.0 1.0        -- ~ 0.785398 (34 iterations)

-- Exponential and logarithmic
def testExp (x : Float) : Float := Float.exp x - 2.0
def testLog (x : Float) : Float := Float.log x - 1.0

#eval! findRoot testExp 0.0 1.0        -- ~ 0.693147 (34 iterations)
#eval! findRoot testLog 2.0 3.0        -- ~ 2.718282 (34 iterations)

-- rats function
def testRational (x : Float) : Float := 1.0 / x - 0.5

#eval! findRoot testRational 1.0 3.0   -- ~ 2.000000 (34 iterations)

-- abs val
def testAbs (x : Float) : Float := Float.abs (x - 1.5) - 0.5

#eval! findRoot testAbs 0.5 1.5        -- ~ Root: 1.000000 (34 iterations)

-- wide interval
def testWide (x : Float) : Float := x^3 - 8.0

#eval! findRoot testWide (-10.0) 10.0  -- ~ Root: 2.000000 (35 iterations)

-- narrow interval
def testNarrow (x : Float) : Float := x * x - 2.0

#eval! findRoot testNarrow 1.4 1.5     -- ~ Root: 1.414214 (28 iterations)

-- High precision
def testHighPrec (x : Float) : Float := x * x - 2.0
def highPrecParams : BisectionParams Float := { tolerance := 1e-15, maxIterations := 200 }

#eval! bisection testHighPrec 1.0 2.0 highPrecParams  -- ~ Root: 1.414214 (50 iterations)

/- Using rational numbers  -/

def testSqrt2Rat (x : ℚ) : ℚ := x * x - 2
def testCubicRat (x : ℚ) : ℚ := x * x * x - x - 1

#eval! findRootRat testSqrt2Rat 1 2    -- ~ Root: 1.414214 (34 iterations)
#eval! findRootRat testCubicRat 1 2    -- ~ Root: 1.324718 (34 iterations)

/- Error Case -/

-- Invalid bounds (a > b)
#eval! findRoot testSqrt2 2.0 1.0      -- left bound must be less than right bound

-- No sign change
#eval! findRoot testSqrt2 2.0 3.0      -- function must have opposite signs at bounds

/- Real Tests (for proofs only not computatiion)  -/

noncomputable def testRealQuadratic : BisectionResult ℝ :=
  bisection (fun x => x^2 - 2) 1 2 { tolerance := 1/1000000, maxIterations := 100 }

noncomputable def testRealCubic : BisectionResult ℝ :=
  bisection (fun x => x^3 - x - 1) 1 2 { tolerance := 1/1000000, maxIterations := 100 }

noncomputable def testRealSin : BisectionResult ℝ :=
  bisection (fun x => Real.sin x) 3 4 { tolerance := 1/1000000, maxIterations := 100 }

noncomputable def testRealCos : BisectionResult ℝ :=
  bisection (fun x => Real.cos x) 1 2 { tolerance := 1/1000000, maxIterations := 100 }

noncomputable def testRealExp : BisectionResult ℝ :=
  bisection (fun x => Real.exp x - 2) 0 1 { tolerance := 1/1000000, maxIterations := 100 }

noncomputable def testRealLog : BisectionResult ℝ :=
  bisection (fun x => Real.log x - 1) 2 3 { tolerance := 1/1000000, maxIterations := 100 }
