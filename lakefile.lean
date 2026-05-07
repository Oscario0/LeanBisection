import Lake
open Lake DSL

package «LeanBisection» where
  -- Lean Bisection numerical method package

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

require RealLike from git
  "https://github.com/Oscario0/Archive" @ "main" / "RealLike"

@[default_target]
lean_lib «Bisection» where
  srcDir := "src"
  roots := #[`bisection, `bisection_tests, `real_props, `hybrid_gate_backend, `certified_bisection, `hybrid_gate_tests]
