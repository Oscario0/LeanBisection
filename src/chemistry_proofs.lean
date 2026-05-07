import Mathlib.Tactic
import bisection

namespace CompressibilityFactor

/-- User-facing parameters for a Van der Waals root-finding problem. -/
structure VanDerWaalsParams (α : Type) where
  A : α
  B : α

/-- Bracketing interval for a one-variable equation. -/
structure RootInterval (α : Type) where
  left : α
  right : α

section

open scoped RealLike

/-- Van der Waals cubic in the compressibility factor `Z`. -/
def vanDerWaalsEquation {α : Type} [RealLike α] (params : VanDerWaalsParams α) :
    α → α :=
  fun Z => Z * Z * Z - (1 + params.B) * Z * Z + params.A * Z - params.A * params.B

end

/--
Single source of truth for the nitrogen example parameters.

This is polymorphic over any type with standard scientific-literal support, so the
same values can be specialized to `Float` for execution and to `ℝ` for proofs.
-/
def nitrogenParams {α : Type} [OfScientific α] : VanDerWaalsParams α where
  A := 0.42
  B := 0.08

/-- Nitrogen Van der Waals cubic, specialized from the shared nitrogen parameters. -/
def nitrogenVdwEquation {α : Type} [RealLike α] [OfScientific α] : α → α :=
  vanDerWaalsEquation (nitrogenParams (α := α))

/-- Shared CO2 Van der Waals parameters. -/
def co2Params {α : Type} [OfScientific α] : VanDerWaalsParams α where
  A := 0.65
  B := 0.09

/-- CO2 Van der Waals cubic, specialized from the shared CO2 parameters. -/
def co2VdwEquation {α : Type} [RealLike α] [OfScientific α] : α → α :=
  vanDerWaalsEquation (co2Params (α := α))

/-- Nitrogen parameters specialized to `ℝ` for theorem-facing proofs. -/
noncomputable abbrev nitrogenParamsR : VanDerWaalsParams ℝ :=
  nitrogenParams

/-- Shared nitrogen interval. -/
def nitrogenInterval {α : Type} [OfScientific α] : RootInterval α where
  left := 0.1
  right := 2.0

/-- Nitrogen Van der Waals equation specialized to `ℝ` for theorem-facing proofs. -/
noncomputable abbrev nitrogenVdwEquationR : ℝ → ℝ :=
  nitrogenVdwEquation

/-- CO2 parameters specialized to `ℝ` for theorem-facing proofs. -/
noncomputable abbrev co2ParamsR : VanDerWaalsParams ℝ :=
  co2Params

/-- Shared CO2 interval. -/
def co2Interval {α : Type} [OfScientific α] : RootInterval α where
  left := 0.1
  right := 2.0

/-- CO2 Van der Waals equation specialized to `ℝ` for theorem-facing proofs. -/
noncomputable abbrev co2VdwEquationR : ℝ → ℝ :=
  co2VdwEquation

/-- Generic continuity lemma for the Van der Waals cubic over `ℝ`. -/
lemma vanDerWaalsEquation_eq_real (params : VanDerWaalsParams ℝ) :
    vanDerWaalsEquation params =
      (fun Z : ℝ =>
        Z * Z * Z
          - ((1 : ℝ) + params.B) * Z * Z
          + params.A * Z
          - params.A * params.B) := by
  funext Z
  change
    Z * Z * Z - (((RealLike.ofNat 1 : ℝ)) + params.B) * Z * Z + params.A * Z - params.A * params.B =
      Z * Z * Z - ((1 : ℝ) + params.B) * Z * Z + params.A * Z - params.A * params.B
  norm_num [RealLike.ofNat]

lemma vanDerWaalsEquation_continuous (params : VanDerWaalsParams ℝ) :
    Continuous (vanDerWaalsEquation params) := by
  rw [vanDerWaalsEquation_eq_real]
  fun_prop

/--
Root existence for a Van der Waals interval once the endpoint signs are known.

This packages the reusable part of the proof: continuity comes from the generic
polynomial lemma, while each concrete example only needs to supply the interval
ordering and endpoint sign facts.
-/
lemma vanDerWaals_root_exists_of_nonpos_nonneg
    (params : VanDerWaalsParams ℝ)
    (interval : RootInterval ℝ)
    (hord : interval.left < interval.right)
    (hleft : vanDerWaalsEquation params interval.left ≤ (0 : ℝ))
    (hright : (0 : ℝ) ≤ vanDerWaalsEquation params interval.right) :
    ∃ x ∈ Set.Icc interval.left interval.right, vanDerWaalsEquation params x = 0 := by
  exact Bisection.existsRootOfContinuousNonposNonneg
    hord (vanDerWaalsEquation_continuous params).continuousOn hleft hright

/--
Certificate constructor for a Van der Waals interval from endpoint sign facts.
-/
lemma vanDerWaals_certificate_of_nonpos_nonneg
    (params : VanDerWaalsParams ℝ)
    (interval : RootInterval ℝ)
    (hord : interval.left < interval.right)
    (hleft : vanDerWaalsEquation params interval.left ≤ (0 : ℝ))
    (hright : (0 : ℝ) ≤ vanDerWaalsEquation params interval.right) :
    Bisection.Certificate
      { f := vanDerWaalsEquation params, left := interval.left, right := interval.right } := by
  exact Bisection.certificateOfContinuousNonposNonneg
    (vanDerWaalsEquation params) interval.left interval.right
    hord (vanDerWaalsEquation_continuous params).continuousOn hleft hright

noncomputable abbrev nitrogenIntervalR : RootInterval ℝ := nitrogenInterval
noncomputable abbrev co2IntervalR : RootInterval ℝ := co2Interval

lemma nitrogen_ordered : nitrogenIntervalR.left < nitrogenIntervalR.right := by
  unfold nitrogenIntervalR nitrogenInterval
  norm_num

lemma nitrogen_left_eval :
    nitrogenVdwEquationR nitrogenIntervalR.left = (-0.0014 : ℝ) := by
  unfold nitrogenVdwEquationR nitrogenVdwEquation
  rw [vanDerWaalsEquation_eq_real]
  norm_num [nitrogenVdwEquationR, nitrogenVdwEquation, nitrogenParamsR, nitrogenParams,
    nitrogenIntervalR, nitrogenInterval]

lemma nitrogen_right_eval :
    nitrogenVdwEquationR nitrogenIntervalR.right = (4.4864 : ℝ) := by
  unfold nitrogenVdwEquationR nitrogenVdwEquation
  rw [vanDerWaalsEquation_eq_real]
  norm_num [nitrogenVdwEquationR, nitrogenVdwEquation, nitrogenParamsR, nitrogenParams,
    nitrogenIntervalR, nitrogenInterval]

lemma nitrogen_continuous :
    ContinuousOn nitrogenVdwEquationR (Set.Icc nitrogenIntervalR.left nitrogenIntervalR.right) := by
  simpa [nitrogenVdwEquationR] using
    (vanDerWaalsEquation_continuous nitrogenParamsR).continuousOn

lemma nitrogen_root_exists :
    ∃ x ∈ Set.Icc nitrogenIntervalR.left nitrogenIntervalR.right, nitrogenVdwEquationR x = 0 := by
  have hleft : vanDerWaalsEquation nitrogenParamsR nitrogenIntervalR.left ≤ 0 := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [nitrogenParamsR, nitrogenParams, nitrogenIntervalR, nitrogenInterval]
  have hright : 0 ≤ vanDerWaalsEquation nitrogenParamsR nitrogenIntervalR.right := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [nitrogenParamsR, nitrogenParams, nitrogenIntervalR, nitrogenInterval]
  simpa [nitrogenVdwEquationR] using
    vanDerWaals_root_exists_of_nonpos_nonneg
      nitrogenParamsR nitrogenIntervalR nitrogen_ordered hleft hright

lemma nitrogenCertificate :
    Bisection.Certificate
      { f := nitrogenVdwEquationR
        left := nitrogenIntervalR.left
        right := nitrogenIntervalR.right } := by
  have hleft : vanDerWaalsEquation nitrogenParamsR nitrogenIntervalR.left ≤ 0 := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [nitrogenParamsR, nitrogenParams, nitrogenIntervalR, nitrogenInterval]
  have hright : 0 ≤ vanDerWaalsEquation nitrogenParamsR nitrogenIntervalR.right := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [nitrogenParamsR, nitrogenParams, nitrogenIntervalR, nitrogenInterval]
  simpa [nitrogenVdwEquationR] using
    vanDerWaals_certificate_of_nonpos_nonneg
      nitrogenParamsR nitrogenIntervalR nitrogen_ordered hleft hright

lemma co2_ordered : co2IntervalR.left < co2IntervalR.right := by
  unfold co2IntervalR co2Interval
  norm_num

lemma co2_left_eval :
    co2VdwEquationR co2IntervalR.left = (-0.0034 : ℝ) := by
  unfold co2VdwEquationR co2VdwEquation
  rw [vanDerWaalsEquation_eq_real]
  norm_num [co2VdwEquationR, co2VdwEquation, co2ParamsR, co2Params, co2IntervalR, co2Interval]

lemma co2_right_eval :
    co2VdwEquationR co2IntervalR.right = (4.8815 : ℝ) := by
  unfold co2VdwEquationR co2VdwEquation
  rw [vanDerWaalsEquation_eq_real]
  norm_num [co2VdwEquationR, co2VdwEquation, co2ParamsR, co2Params, co2IntervalR, co2Interval]

lemma co2_continuous :
    ContinuousOn co2VdwEquationR (Set.Icc co2IntervalR.left co2IntervalR.right) := by
  simpa [co2VdwEquationR] using
    (vanDerWaalsEquation_continuous co2ParamsR).continuousOn

lemma co2_root_exists :
    ∃ x ∈ Set.Icc co2IntervalR.left co2IntervalR.right, co2VdwEquationR x = 0 := by
  have hleft : vanDerWaalsEquation co2ParamsR co2IntervalR.left ≤ 0 := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [co2ParamsR, co2Params, co2IntervalR, co2Interval]
  have hright : 0 ≤ vanDerWaalsEquation co2ParamsR co2IntervalR.right := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [co2ParamsR, co2Params, co2IntervalR, co2Interval]
  simpa [co2VdwEquationR] using
    vanDerWaals_root_exists_of_nonpos_nonneg
      co2ParamsR co2IntervalR co2_ordered hleft hright

lemma co2Certificate :
    Bisection.Certificate
      { f := co2VdwEquationR, left := co2IntervalR.left, right := co2IntervalR.right } := by
  have hleft : vanDerWaalsEquation co2ParamsR co2IntervalR.left ≤ 0 := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [co2ParamsR, co2Params, co2IntervalR, co2Interval]
  have hright : 0 ≤ vanDerWaalsEquation co2ParamsR co2IntervalR.right := by
    rw [vanDerWaalsEquation_eq_real]
    norm_num [co2ParamsR, co2Params, co2IntervalR, co2Interval]
  simpa [co2VdwEquationR] using
    vanDerWaals_certificate_of_nonpos_nonneg
      co2ParamsR co2IntervalR co2_ordered hleft hright

/-- User-facing parameters for a Redlich-Kwong root-finding problem. -/
structure RedlichKwongParams (α : Type) where
  A : α
  B : α

section

open scoped RealLike

/-- Redlich-Kwong cubic in the compressibility factor `Z`. -/
def redlichKwongEquation {α : Type} [RealLike α] (params : RedlichKwongParams α) :
    α → α :=
  fun Z => Z * Z * Z - Z * Z + (params.A - params.B - params.B * params.B) * Z - params.A * params.B

end

/-- Shared nitrogen Redlich-Kwong parameters. -/
def nitrogenRKParams {α : Type} [OfScientific α] : RedlichKwongParams α where
  A := 0.5
  B := 0.08

/-- Shared CO2 Redlich-Kwong parameters. -/
def co2RKParams {α : Type} [OfScientific α] : RedlichKwongParams α where
  A := 0.9
  B := 0.1

/-- Nitrogen Redlich-Kwong cubic. -/
def nitrogenRKEquation {α : Type} [RealLike α] [OfScientific α] : α → α :=
  redlichKwongEquation (nitrogenRKParams (α := α))

/-- CO2 Redlich-Kwong cubic. -/
def co2RKEquation {α : Type} [RealLike α] [OfScientific α] : α → α :=
  redlichKwongEquation (co2RKParams (α := α))

noncomputable abbrev nitrogenRKParamsR : RedlichKwongParams ℝ := nitrogenRKParams
noncomputable abbrev co2RKParamsR : RedlichKwongParams ℝ := co2RKParams
noncomputable abbrev nitrogenRKEquationR : ℝ → ℝ := nitrogenRKEquation
noncomputable abbrev co2RKEquationR : ℝ → ℝ := co2RKEquation
noncomputable abbrev nitrogenRKIntervalR : RootInterval ℝ := nitrogenInterval
noncomputable abbrev co2RKIntervalR : RootInterval ℝ := co2Interval

lemma redlichKwongEquation_eq_real (params : RedlichKwongParams ℝ) :
    redlichKwongEquation params =
      (fun Z : ℝ =>
        Z * Z * Z
          - Z * Z
          + (params.A - params.B - params.B * params.B) * Z
          - params.A * params.B) := by
  funext Z
  dsimp [redlichKwongEquation]

lemma redlichKwongEquation_continuous (params : RedlichKwongParams ℝ) :
    Continuous (redlichKwongEquation params) := by
  rw [redlichKwongEquation_eq_real]
  fun_prop

lemma redlichKwong_root_exists_of_nonpos_nonneg
    (params : RedlichKwongParams ℝ)
    (interval : RootInterval ℝ)
    (hord : interval.left < interval.right)
    (hleft : redlichKwongEquation params interval.left ≤ (0 : ℝ))
    (hright : (0 : ℝ) ≤ redlichKwongEquation params interval.right) :
    ∃ x ∈ Set.Icc interval.left interval.right, redlichKwongEquation params x = 0 := by
  exact Bisection.existsRootOfContinuousNonposNonneg
    hord (redlichKwongEquation_continuous params).continuousOn hleft hright

lemma redlichKwong_certificate_of_nonpos_nonneg
    (params : RedlichKwongParams ℝ)
    (interval : RootInterval ℝ)
    (hord : interval.left < interval.right)
    (hleft : redlichKwongEquation params interval.left ≤ (0 : ℝ))
    (hright : (0 : ℝ) ≤ redlichKwongEquation params interval.right) :
    Bisection.Certificate
      { f := redlichKwongEquation params, left := interval.left, right := interval.right } := by
  exact Bisection.certificateOfContinuousNonposNonneg
    (redlichKwongEquation params) interval.left interval.right
    hord (redlichKwongEquation_continuous params).continuousOn hleft hright

lemma nitrogenRK_ordered : nitrogenRKIntervalR.left < nitrogenRKIntervalR.right := by
  unfold nitrogenRKIntervalR nitrogenInterval
  norm_num

lemma nitrogenRK_left_eval :
    nitrogenRKEquationR nitrogenRKIntervalR.left = (-0.00764 : ℝ) := by
  unfold nitrogenRKEquationR nitrogenRKEquation
  rw [redlichKwongEquation_eq_real]
  norm_num [nitrogenRKEquationR, nitrogenRKEquation, nitrogenRKParamsR, nitrogenRKParams,
    nitrogenRKIntervalR, nitrogenInterval]

lemma nitrogenRK_right_eval :
    nitrogenRKEquationR nitrogenRKIntervalR.right = (4.7872 : ℝ) := by
  unfold nitrogenRKEquationR nitrogenRKEquation
  rw [redlichKwongEquation_eq_real]
  norm_num [nitrogenRKEquationR, nitrogenRKEquation, nitrogenRKParamsR, nitrogenRKParams,
    nitrogenRKIntervalR, nitrogenInterval]

lemma nitrogenRKCertificate :
    Bisection.Certificate
      { f := nitrogenRKEquationR
        left := nitrogenRKIntervalR.left
        right := nitrogenRKIntervalR.right } := by
  have hleft : redlichKwongEquation nitrogenRKParamsR nitrogenRKIntervalR.left ≤ 0 := by
    rw [redlichKwongEquation_eq_real]
    norm_num [nitrogenRKParamsR, nitrogenRKParams, nitrogenRKIntervalR, nitrogenInterval]
  have hright : 0 ≤ redlichKwongEquation nitrogenRKParamsR nitrogenRKIntervalR.right := by
    rw [redlichKwongEquation_eq_real]
    norm_num [nitrogenRKParamsR, nitrogenRKParams, nitrogenRKIntervalR, nitrogenInterval]
  simpa [nitrogenRKEquationR] using
    redlichKwong_certificate_of_nonpos_nonneg
      nitrogenRKParamsR nitrogenRKIntervalR nitrogenRK_ordered hleft hright

lemma co2RK_ordered : co2RKIntervalR.left < co2RKIntervalR.right := by
  unfold co2RKIntervalR co2Interval
  norm_num

lemma co2RK_left_eval :
    co2RKEquationR co2RKIntervalR.left = (-0.02 : ℝ) := by
  unfold co2RKEquationR co2RKEquation
  rw [redlichKwongEquation_eq_real]
  norm_num [co2RKEquationR, co2RKEquation, co2RKParamsR, co2RKParams,
    co2RKIntervalR, co2Interval]

lemma co2RK_right_eval :
    co2RKEquationR co2RKIntervalR.right = (5.49 : ℝ) := by
  unfold co2RKEquationR co2RKEquation
  rw [redlichKwongEquation_eq_real]
  norm_num [co2RKEquationR, co2RKEquation, co2RKParamsR, co2RKParams,
    co2RKIntervalR, co2Interval]

lemma co2RKCertificate :
    Bisection.Certificate
      { f := co2RKEquationR, left := co2RKIntervalR.left, right := co2RKIntervalR.right } := by
  have hleft : redlichKwongEquation co2RKParamsR co2RKIntervalR.left ≤ 0 := by
    rw [redlichKwongEquation_eq_real]
    norm_num [co2RKParamsR, co2RKParams, co2RKIntervalR, co2Interval]
  have hright : 0 ≤ redlichKwongEquation co2RKParamsR co2RKIntervalR.right := by
    rw [redlichKwongEquation_eq_real]
    norm_num [co2RKParamsR, co2RKParams, co2RKIntervalR, co2Interval]
  simpa [co2RKEquationR] using
    redlichKwong_certificate_of_nonpos_nonneg
      co2RKParamsR co2RKIntervalR co2RK_ordered hleft hright

end CompressibilityFactor
