import Mathlib

class BisectionConfig (α : Type*) where
  add : α → α → α
  sub : α → α → α
  div : α → α → α
  mul : α → α → α
  lt : α → α → Bool
  le : α → α → Bool
  zero : α
  two : α
  ---- : α
  isPositive : α → Bool
  isNegative : α → Bool
  -- valid : left - right

instance [BisectionConfig α] : Add α where add := BisectionConfig.add
instance [BisectionConfig α] : Sub α where sub := BisectionConfig.sub
instance [BisectionConfig α] : Div α where div := BisectionConfig.div
instance  [BisectionConfig α] : Mul α where mul := BisectionConfig.mul

instance : BisectionConfig Float where
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt := (· < ·)
  le := (· ≤ ·)
  zero := 0.0
  two := 2.0
  -- := 4.0
  isPositive x := x > 0.0
  isNegative x := x < 0.0

instance : BisectionConfig ℚ where
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt := (· < ·)
  le := (· ≤ · )
  zero := 0
  two := 2
  isPositive x := x > 0.0
  isNegative x := x < 0.0
noncomputable instance : BisectionConfig ℝ where
  add := (· + ·)
  sub := (· - ·)
  div := (· / ·)
  mul := (· * ·)
  lt x y := decide (x < y)
  le x y := decide (x ≤ y)
  zero := 0
  two := 2
  -- := 4
  isPositive x := decide (x > 0)
  isNegative x := decide (x < 0)

def bisectionCore {α : Type*} [BisectionConfig α ]
    (f : α → α) (a b tolerance : α) (maxIter : ℕ ) : Option α :=
  if BisectionConfig.le b a then none  -- a >= b
  else
    let rec loop (left right : α) (iter : ℕ) : Option α :=
      if iter >= maxIter then
        some ((left + right) / BisectionConfig.two)
      else if BisectionConfig.lt (right - left) tolerance then
        some ((left + right) / BisectionConfig.two)
      else
        let mid := (left + right) / BisectionConfig.two
        let fleft := f left
        let fmid := f mid
        if (BisectionConfig.isPositive fleft && BisectionConfig.isNegative fmid) ||
            (BisectionConfig.isNegative fleft && BisectionConfig.isPositive fmid) then
          loop left mid (iter + 1)
        else
          loop mid right (iter + 1)
      termination_by maxIter - iter
    loop a b 0
