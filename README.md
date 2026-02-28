# Lean Bisection Method

A polymorphic implementation of the bisection root-finding algorithm in Lean 4, supporting Float and ℝ.

## Overview

The bisection method is a robust numerical algorithm for finding roots of continuous functions. This implementation provides:

- **Polymorphic design** via `BisectionField` typeclass - works with Float, ℚ, and ℝ
- **Configurable parameters** (tolerance, max iterations (optional))
- **Formal proofs** of termination and boundedness (see `real_props.lean`)

### Core Components

````lean4
-- Result type with detailed outcomes
inductive BisectionResult (α : Type*) where
  | success (root : α) (iterations : Nat)
  | invalidBounds (reason : String)
  | noSignChange (reason : String)
  | maxIterationsReached (bestApprox : α) (iterations : Nat)

-- Typeclass for numeric types supporting bisection using basic Mathlib definitions
class BisectionField (α : Type*) where
  zero : α
  two : α
  add sub mul div : α → α → α
  lt le : α → α → Bool
  abs : α → α
  isPositive isNegative : α → Bool
  toFloat : α → Float  -- for display

-- Configuration
structure BisectionParams (α : Type*) where
  tolerance : α
  maxIterations : Nat := 1000

-- Core algorithm (polymorphic)
def bisection {α : Type*} [BisectionField α]
    (f : α → α) (a b : α) (params : BisectionParams α) : BisectionResult α

## Usage

### Running the Implementation

```bash
cd LeanBisection
lake build Bisection
````

### Float Examples

```lean4
-- Define test functions
def testSqrt2 (x : Float) : Float := x * x - 2.0
def testCubic (x : Float) : Float := x * x * x - x - 1.0
def testPi (x : Float) : Float := Float.sin x

-- Find roots with default parameters
#eval! findRoot testSqrt2 1.0 2.0   -- ✓ Root: 1.414214 (29 iterations)
#eval! findRoot testCubic 1.0 2.0   -- ✓ Root: 1.324718 (34 iterations)
#eval! findRoot testPi 3.0 4.0      -- ✓ Root: 3.141593 (33 iterations)

-- With custom configuration
def highPrecParams : BisectionParams Float := { tolerance := 1e-15, maxIterations := 200 }
#eval! bisection testSqrt2 1.0 2.0 highPrecParams  -- ✓ Root: 1.414214 (50 iterations)
```

#### Float Limitations

Bisection can still "succeed" in cases where no true root exists due to floating-point behavior:

```lean4
-- Pole crossing: 1/x on [-1, 1] has no root, but bisection finds x ≈ 0
def testPoleCross (x : Float) : Float := 1.0 / x
#eval! findRoot testPoleCross (-1.0) 1.0  -- ✓ Root: -0.000000 (no real root)

-- High powers: (x-1)^11 has root at x=1, but Float precision degrades
def testPower (x : Float) : Float := (x - 1)^11
#eval! findRoot testPower 0.9 1.5  -- not going to find exactly 1.0
```

### Rational (ℚ) Examples

```lean4
def testSqrt2Rat (x : ℚ) : ℚ := x * x - 2

#eval! findRootRat testSqrt2Rat 1 2  -- ✓ Root: 1.414214 (29 iterations)
```

### Real (ℝ) Examples (noncomputable, for proofs)

```lean4
noncomputable def testRealQuadratic : BisectionResult ℝ :=
  bisection (fun x => x^2 - 2) 1 2 { tolerance := 1/1000000, maxIterations := 100 }
```

### Error Handling

```lean4
#eval! findRoot testSqrt2 2.0 1.0  -- ✗ Invalid bounds: left bound must be less than right bound
#eval! findRoot testSqrt2 2.0 3.0  -- ✗ No sign change: function must have opposite signs at bounds
```

## Formal Proofs

The `src/real_props.lean` file contains formal proofs for the bisection algorithm:

- **Interval halving**: Each iteration halves the search interval
- **Termination and boundedness**: The result is always within [a, b]

```lean4
theorem bisection_termination_bounded_real
    (f : ℝ → ℝ) (a b tolerance : ℝ) (maxIter : ℕ)
    (h_order : a < b) (h_tol_pos : 0 < tolerance) :
  ∃ x : ℝ, result = some x ∧ a ≤ x ∧ x ≤ b ∧
    (∃ n : ℕ, n ≤ maxIter ∧ |x - ((a + b) / 2)| ≤ (b - a) / 2^(n + 1))
```

- Our proofs will be updated moving forward as the code grows

## How It Works

1. **Validate bounds**: Check that `a < b` and `f(a)`, `f(b)` have opposite signs
2. **Iterate**: Repeatedly bisect the interval at the midpoint
3. **Converge**: Stop when interval width or |f(mid)| is below tolerance

## Project Structure

```
LeanBisection/
├── src/
│   ├── bisection.lean       # Core polymorphic implementation
│   ├── bisection_tests.lean # Test suite
│   └── real_props.lean      # Formal proofs
├── lakefile.lean
└── README.md
```

## Requirements

- **Lean 4**: v4.27.0-rc1 or compatible
- **Mathlib**
- **Lake**:
