# Lean Bisection Method

A practical implementation of the bisection root-finding algorithm in Lean 4 using Float arithmetic.

## Overview

The bisection method is a robust numerical algorithm for finding roots of continuous functions. This implementation provides:

- **Float-based computation** for practical numerical results
- **Configurable parameters** (tolerance, max iterations)
- **Comprehensive error handling** with detailed result types
- **Built-in test functions** for common mathematical equations

## Implementation: bisection.lean

The complete implementation is contained in a single file: `src/bisection.lean`

### Core Components

The implementation includes:
- `BisectionResult` - Result type with success/error cases
- `BisectionConfig` - Configuration for tolerance and iteration limits  
- `bisection` - Main algorithm implementation
- `findRoot` - Convenient wrapper function
- Built-in test functions and examples

### Data Types

```lean4
inductive BisectionResult where
  | success (root : Float) (iterations : Nat)
  | invalidBounds (reason : String)  
  | maxIterationsReached (bestApprox : Float) (iterations : Nat)

structure BisectionConfig where
  tolerance : Float := 1e-10
  maxIterations : Nat := 1000
```

### Algorithm Implementation

- **`bisection`**: Main algorithm with bounds checking and iterative refinement
- **`findRoot`**: Wrapper function using default configuration
- **`oppositeSigns`**: Utility for checking sign changes (root detection)

### Built-in Examples

The file includes three test functions:
- `testFunction1`: x² - 2 = 0 (finds √2)
- `testFunction2`: x³ - x - 1 = 0 (cubic equation)  
- `testFunction3`: sin(x) = 0 (finds π)

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
3. **Converge**: Stop when the interval is smaller than tolerance or function value is near zero
4. **Handle errors**: Return appropriate error messages for invalid inputs

### Algorithm Properties

- **Guaranteed convergence**: Always finds a root if one exists in the interval
- **Predictable iterations**: At most `⌈log₂((b-a)/tolerance)⌉` steps
- **Robust**: Works for any continuous function, no derivatives needed
- **Simple**: Easy to understand and implement correctly

## Goals

This implementation demonstrates:
- Clean, readable Lean 4 code for numerical algorithms
- Practical Float-based computation 
- Proper error handling and result types
- Self-contained example with built-in tests

## Future Work

Potential extensions:
- **Formal proofs**: Add mathematical verification of correctness
- **Generic types**: Extend beyond Float to other numeric types
- **Performance**: Optimize for larger-scale computations
- **Integration**: Connect with Mathlib's analysis library

## Requirements

- **Lean 4**: Any recent version (tested with v4.25.0-rc2)
- **Lake**: For package management
- **No external dependencies**: Pure Lean implementation
