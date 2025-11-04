# Lean Bisection Method

A practical implementation of the bisection root-finding algorithm in Lean 4 using Float arithmetic and Algebra on Real-valued Functions.

## Overview

The bisection method is a robust numerical algorithm for finding roots of continuous functions. This implementation provides:

- **Float-based computation** for practical numerical results
- **Configurable parameters** (tolerance, max iterations (optional))
- **Comprehensive error handling** with detailed result types
- **Built-in test functions** for common mathematical equations and more complex trigonometric functions and algebraic equations

## Implementation: bisection.lean

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

## Goals

This implementation aims at performing practical and complex numerical computations in Lean and ensure results formally verified without a "human-in-the-loop" 

## Future Work

- **Generic types**: Currently extending the code beyond Floats to handle other numeric types
- **Formal proofs**: Add mathematical verification using proofs for code correctness
- **Performance**: Optimize for faster execution

## Requirements

- **Lean 4**: Any recent version (tested with v4.25.0-rc2)
- **Lake**: For package management

