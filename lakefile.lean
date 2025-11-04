import Lake
open Lake DSL

package «LeanBisection» where
  -- Lean Bisection numerical method package

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

@[default_target]
lean_lib «bisection» where
  srcDir := "src"
