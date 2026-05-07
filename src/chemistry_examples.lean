import bisection
import ChemistryProofs

open scoped RealLike
open Bisection

namespace CompressibilityFactor

/-- Execute a Van der Waals problem over `Float` using its stored interval. -/
def runVanDerWaals (params : VanDerWaalsParams Float) (interval : RootInterval Float) :
    BisectionResult Float :=
  Bisection.bisectionCore (vanDerWaalsEquation params) interval.left interval.right
    { tolerance := 0.0001, maxIterations := 100 }

/-- Certified executable Van der Waals run from matched real and Float parameter bundles. -/
def certifiedRunVanDerWaals
    (paramsR : VanDerWaalsParams ℝ)
    (intervalR : RootInterval ℝ)
    (paramsF : VanDerWaalsParams Float)
    (intervalF : RootInterval Float)
    (_cert : Bisection.Certificate
      { f := vanDerWaalsEquation paramsR, left := intervalR.left, right := intervalR.right }) :
    Except String (BisectionResult Float) :=
  Bisection.bisectionCertifiedOfFunctions?
    (vanDerWaalsEquation paramsR)
    intervalR.left
    intervalR.right
    (vanDerWaalsEquation paramsF)
    intervalF.left
    intervalF.right
    _cert
    { tolerance := 0.0001, maxIterations := 100 }

def nitrogenExample : BisectionResult Float :=
  runVanDerWaals (nitrogenParams (α := Float)) (nitrogenInterval (α := Float))

def certifiedNitrogenExample : Except String (BisectionResult Float) :=
  certifiedRunVanDerWaals
    nitrogenParamsR
    nitrogenIntervalR
    (nitrogenParams (α := Float))
    (nitrogenInterval (α := Float))
    CompressibilityFactor.nitrogenCertificate

def co2Example : BisectionResult Float :=
  runVanDerWaals (co2Params (α := Float)) (co2Interval (α := Float))

def certifiedCo2Example : Except String (BisectionResult Float) :=
  certifiedRunVanDerWaals
    co2ParamsR
    co2IntervalR
    (co2Params (α := Float))
    (co2Interval (α := Float))
    CompressibilityFactor.co2Certificate

/-- Execute a Redlich-Kwong problem over `Float` using its stored interval. -/
def runRedlichKwong (params : RedlichKwongParams Float) (interval : RootInterval Float) :
    BisectionResult Float :=
  Bisection.bisectionCore (redlichKwongEquation params) interval.left interval.right
    { tolerance := 0.0001, maxIterations := 100 }

/-- Certified executable Redlich-Kwong run from matched real and Float parameter bundles. -/
def certifiedRunRedlichKwong
    (paramsR : RedlichKwongParams ℝ)
    (intervalR : RootInterval ℝ)
    (paramsF : RedlichKwongParams Float)
    (intervalF : RootInterval Float)
    (_cert : Bisection.Certificate
      { f := redlichKwongEquation paramsR, left := intervalR.left, right := intervalR.right }) :
    Except String (BisectionResult Float) :=
  Bisection.bisectionCertifiedOfFunctions?
    (redlichKwongEquation paramsR)
    intervalR.left
    intervalR.right
    (redlichKwongEquation paramsF)
    intervalF.left
    intervalF.right
    _cert
    { tolerance := 0.0001, maxIterations := 100 }

def nitrogenRKExample : BisectionResult Float :=
  runRedlichKwong (nitrogenRKParams (α := Float)) (nitrogenInterval (α := Float))

def certifiedNitrogenRKExample : Except String (BisectionResult Float) :=
  certifiedRunRedlichKwong
    nitrogenRKParamsR
    nitrogenRKIntervalR
    (nitrogenRKParams (α := Float))
    (nitrogenInterval (α := Float))
    CompressibilityFactor.nitrogenRKCertificate

def co2RKExample : BisectionResult Float :=
  runRedlichKwong (co2RKParams (α := Float)) (co2Interval (α := Float))

def certifiedCo2RKExample : Except String (BisectionResult Float) :=
  certifiedRunRedlichKwong
    co2RKParamsR
    co2RKIntervalR
    (co2RKParams (α := Float))
    (co2Interval (α := Float))
    CompressibilityFactor.co2RKCertificate

#eval nitrogenExample
#eval certifiedNitrogenExample
#eval co2Example
#eval certifiedCo2Example
#eval nitrogenRKExample
#eval certifiedNitrogenRKExample
#eval co2RKExample
#eval certifiedCo2RKExample

end CompressibilityFactor
