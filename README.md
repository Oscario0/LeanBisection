# Lean Bisection Method

A practical implementation of the bisection root-finding algorithm in Lean 4 using Float arithmetic and Algebra on Real-valued Functions.

## Overview

The bisection method is a robust numerical algorithm for finding roots of continuous functions. This implementation provides:

- **Float-based computation** for practical numerical results
- **Configurable parameters** (tolerance, max iterations (optional))
- **Comprehensive error handling** with detailed result types
- **Built-in test functions** for common mathematical equations and more complex trigonometric functions and algebraic equations

## Implementation

The complete implementation using float arithmetic: `src/bisection.lean`

### Core Components

The implementation includes:

- `BisectionResult` - Result type with success/error cases
- `BisectionConfig` - Configuration for tolerance and iteration limits
- `bisection` - Main loop
- `findRoot` - Convenient wrapper function
- Built-in test functions, examples and so on

## Usage

### Running the Implementation

```bash
# Execute the bisection.lean file directly
cd LeanBisection
lake env lean --run src/bisection.lean

# Or build the Lake package
lake build
```

### Example Usage

The file contains ready-to-run examples:

```lean4
-- Built-in test functions
def testFunction1 (x : Float) : Float := x * x - 2.0        -- √2
def testFunction2 (x : Float) : Float := x * x * x - x - 1.0 -- cubic
def testFunction3 (x : Float) : Float := Float.sin x        -- π

-- Execute with #eval!
#eval! findRoot testFunction1 1.0 2.0  -- Finds √2 ≈ 1.414
#eval! findRoot testFunction2 1.0 2.0  -- Finds cubic root ≈ 1.325
#eval! findRoot testFunction3 3.0 4.0  -- Finds π ≈ 3.142
```

### Custom Functions

```lean4
-- Define your own function
def myFunction (x : Float) : Float := x * x - 5.0

-- Find the root
#eval! findRoot myFunction 2.0 3.0  -- Finds √5 ≈ 2.236

-- With custom configuration
let config : BisectionConfig := { tolerance := 1e-15, maxIterations := 2000 }
#eval! bisection myFunction 2.0 3.0 config
```

## How It Works

The bisection method finds roots by:

1. **Validate bounds**: Check that `a < b` and `f(a)` and `f(b)` have opposite signs
2. **Iterate**: Repeatedly bisect the interval `[a,b]` at the midpoint
3. **Converge**: Stop when the interval is smaller than tolerance or function value is near and/or equal zero

## Chemistry Applications

The bisection method is particularly useful for solving equations of state in chemical engineering. See `src/chemistry_examples.lean` for examples.

### Compressibility Factor (Z)

The compressibility factor Z relates real gas behavior to ideal gas behavior via: **PV = ZnRT**

#### Van der Waals Equation

For the van der Waals equation of state, Z satisfies: **Z³ - (1 + B)Z² + AZ - AB = 0**

where A = aP/(RT)² and B = bP/(RT)

```lean4
-- Van der Waals equation solver
def vanDerWaalsEquation (A B : Float) (Z : Float) : Float :=
  Z * Z * Z - (1.0 + B) * Z * Z + A * Z - A * B

-- Example: Calculate Z for nitrogen at reduced conditions
def nitrogenExample : Option Float :=
  let A := 0.42  -- Reduced parameter aP/(RT)²
  let B := 0.08  -- Reduced parameter bP/(RT)
  let equation := vanDerWaalsEquation A B
  bisectionCore equation 0.1 2.0 0.0001 100

#eval nitrogenExample  -- Returns compressibility factor
```

#### Redlich-Kwong Equation

For the Redlich-Kwong equation: **Z³ - Z² + (A - B - B²)Z - AB = 0**

```lean4
def redlichKwongEquation (A B : Float) (Z : Float) : Float :=
  Z * Z * Z - Z * Z + (A - B - B * B) * Z - A * B

def redlichKwongExample : Option Float :=
  let A := 0.5
  let B := 0.08
  let equation := redlichKwongEquation A B
  bisectionCore equation 0.1 2.0 0.0001 100
```

## Goals

This implementation aims at performing practical and complex numerical computations in Lean and ensure results formally verified without a "human-in-the-loop"

## Future Work

- **Generic types**: Currently extending the code beyond Floats to handle other numeric types
- **Formal proofs**: Add mathematical verification using proofs for code correctness
- **Performance**: Optimize for faster execution

## Requirements

- **Lean 4**: Any recent version (tested with v4.25.0-rc2)
- **Lake**: For package management
