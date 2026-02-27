import bisection


def testSqrt2Poly {α : Type*} [BisectionField α] (x : α) : α := x * x - 2

#eval! bisectionCore testSqrt2Poly (1.0 : Float) (2.0 : Float) defaultParamsFloat
#eval! bisectionCore testSqrt2Poly (1 : ℚ) (2 : ℚ) defaultParamsRat

/-- Polymorphic cubic: x³ - x - 1 -/
def testCubicPoly {α : Type*} [BisectionField α] (x : α) : α := x * x * x - x - 1

#eval! bisectionCore testCubicPoly (1.0 : Float) (2.0 : Float) defaultParamsFloat
#eval! bisectionCore testCubicPoly (1 : ℚ) (2 : ℚ) defaultParamsRat

/-- Polymorphic inverse: 1/x - 1/2 -/
def testInversePoly {α : Type*} [BisectionField α] (x : α) : α := 1 / x - (1 / 2)
#eval! bisectionCore testInversePoly (0.5 : Float) (4.0 : Float) defaultParamsFloat
#eval! bisectionCore testInversePoly (1 : ℚ) (4 : ℚ) defaultParamsRat

/- ===== Float Tests ===== -/

def testSqrt2 (x : Float) : Float := x * x - 2.0
def testCubic (x : Float) : Float := x * x * x - x - 1.0
def testQuartic (x : Float) : Float := x^4 - 5*x^2 + 4.0

#eval! findRoot testSqrt2 1.0 2.0      -- ~ 1.414214
#eval! findRoot testCubic 1.0 2.0      -- ~ 1.324718
#eval! findRoot testQuartic 1.5 2.5    -- ~ 2.000000

-- transcendentals
def testPi (x : Float) : Float := Float.sin x
def testSinCos (x : Float) : Float := Float.sin x - Float.cos x
def testCos (x : Float) : Float := Float.cos x
def testTan (x : Float) : Float := Float.tan x - 1.0

#eval! findRoot testPi 3.0 4.0
#eval! findRoot testSinCos 0.5 1.0
#eval! findRoot testCos 1.0 2.0
#eval! findRoot testTan 0.0 1.0

-- Exponential and logarithmic
def testExp (x : Float) : Float := Float.exp x - 2.0
def testLog (x : Float) : Float := Float.log x - 1.0

#eval! findRoot testExp 0.0 1.0        -- ~ 0.693147
#eval! findRoot testLog 2.0 3.0        -- ~ 2.718282

-- rats function
def testRational (x : Float) : Float := 1.0 / x - 0.5

#eval! findRoot testRational 1.0 3.0   -- ~ 2.000000

-- abs val
def testAbs (x : Float) : Float := Float.abs (x - 1.5) - 0.5

#eval! findRoot testAbs 0.5 1.5        -- ~ Root: 1.000000

-- wide interval
def testWide (x : Float) : Float := x^3 - 8.0

#eval! findRoot testWide (-10.0) 10.0  -- ~ Root: 2.000000

-- narrow interval
def testNarrow (x : Float) : Float := x * x - 2.0

#eval! findRoot testNarrow 1.4 1.5     -- ~ Root: 1.414214

-- High precision
def testHighPrec (x : Float) : Float := x * x - 2.0
def highPrecParams : BisectionParams Float := { tolerance := 1e-15, maxIterations := 200 }

#eval! bisectionCore testHighPrec 1.0 2.0 highPrecParams  -- ~ Root: 1.414214

/- Egde Case - Limitations working w/ Floats-/

-- 1/x on [-1, 1] - converges but a real root does not exists at x=0
def testRootCross (x : Float) : Float := 1.0 / x
#eval! findRoot testRootCross (-1.0) 1.0 ~ Root: -0.000000

-- (x-1)^n - converges, but the root can't be accurately represented in Float for large n
instance : Pow Float Float where pow := Float.pow
def testPower (x : Float) : Float := (x - 1)^11

#eval! findRoot testPower 0.9 1.5
#eval! findRoot testPower 0.9 1.1
#eval! findRoot testPower 0.9 1.05
#eval! findRoot testPower 0.1 99

/- Error Case -/

-- Invalid bounds (a > b)
#eval! findRoot testSqrt2 2.0 1.0

-- No sign change
#eval! findRoot testSqrt2 2.0 3.0

/- Using rational numbers  -/

def testSqrt2Rat (x : ℚ) : ℚ := x * x - 2
def testCubicRat (x : ℚ) : ℚ := x * x * x - x - 1

#eval! findRootRat testSqrt2Rat 1 2    -- ~ Root: 1.414214
#eval! findRootRat testCubicRat 1 2    -- ~ Root: 1.324718


noncomputable def testRealQuadratic : BisectionResult ℝ :=
  bisectionCore testSqrt2Poly 1 2 { tolerance := 1/1000000, maxIterations := 100 }

noncomputable def testRealCubic : BisectionResult ℝ :=
  bisectionCore testCubicPoly 1 2 { tolerance := 1/1000000, maxIterations := 100 }

noncomputable def testRealInverse : BisectionResult ℝ :=
  bisectionCore testInversePoly 1 4 { tolerance := 1/1000000, maxIterations := 100 }
