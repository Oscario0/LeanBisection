import bisection


-- The compressibility factor Z satisfies: PV = ZnRT
-- For van der Waals equation: (P + a/V²)(V - b) = RT
-- Rearranging for Z: Z³ - (1 + B)Z² + AZ - AB = 0
-- where A = aP/(RT)², B = bP/(RT)
namespace CompressibilityFactor

-- Van der Waals equation solver
-- We solve: Z³ - (1 + B)Z² + AZ - AB = 0
def vanDerWaalsEquation (A B : Float) (Z : Float) : Float :=
  Z * Z * Z - (1.0 + B) * Z * Z + A * Z - A * B

-- Example 1: Calculate Z for nitrogen at reduced conditions
-- Using reduced van der Waals parameters
def nitrogenExample : Option Float :=
  let A := 0.42  -- Reduced parameter aP/(RT)²
  let B := 0.08  -- Reduced parameter bP/(RT)
  let equation := vanDerWaalsEquation A B
  bisectionCore equation 0.1 2.0 0.0001 100

-- Example 2: Ideal gas
def idealGasExample : Option Float :=
  let equation (Z : Float) := Z - 1.0
  bisectionCore equation 0.5 1.5 0.0001 100

-- Example 3: High pressure gas
def highPressureExample : Option Float :=
  let A := 0.2
  let B := 0.15
  let equation := vanDerWaalsEquation A B
  bisectionCore equation 1.0 2.0 0.0001 100

-- Example 4: Low temperature gas
def lowTempExample : Option Float :=
  let A := 1.5
  let B := 0.05
  let equation := vanDerWaalsEquation A B
  bisectionCore equation 0.1 1.0 0.0001 100

-- Redlich-Kwong equation: Z³ - Z² + (A - B - B²)Z - AB = 0
-- where A = aP/(R²T^2.5), B = bP/(RT)
def redlichKwongEquation (A B : Float) (Z : Float) : Float :=
  Z * Z * Z - Z * Z + (A - B - B * B) * Z - A * B

def redlichKwongExample : Option Float :=
  let A := 0.5
  let B := 0.08
  let equation := redlichKwongEquation A B
  bisectionCore equation 0.1 2.0 0.0001 100

def verifyCompressibility (equation : Float → Float) (Z : Float) : Float :=
  equation Z

-- Test all examples
#eval nitrogenExample
#eval idealGasExample
#eval highPressureExample
#eval lowTempExample
#eval redlichKwongExample

end CompressibilityFactor
