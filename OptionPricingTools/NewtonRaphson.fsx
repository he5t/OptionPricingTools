
// Newton Raphson Method
//#r @"C:\Users\Matt\Documents\Visual Studio 2017\Packages\mathnet.numerics.3.17.0\lib\net40\MathNet.Numerics.dll"
//#r @"C:\Users\matth\Documents\Visual Studio 2015\Packages\mathnet.numerics.3.17.0\lib\net40\MathNet.Numerics.dll"

#r @"..\packages\MathNet.Numerics.3.17.0\lib\net40\MathNet.Numerics.dll"
#r @"..\packages\ExcelDna.AddIn.0.33.9\tools\ExcelDna.Integration.dll"

module BlackScholes =

  open MathNet.Numerics.Distributions
  
  let d1 S K r sigma T =
    (log(S/K) + (r + sigma * sigma / 2.0) * T) / (sigma * sqrt(T))

  let d2 S K r sigma T =
    let d1 = d1 S K r sigma T
    d1 - sigma * sqrt(T)

  let vega S K r sigma T =
    let d1 = d1 S K r sigma T
    let n'd1 = 1.0 / sqrt(2.0 * MathNet.Numerics.Constants.Pi) * exp(-d1 * d1 / 2.0)
    S * exp(-r * T) * n'd1 * sqrt(T)

  let call S K r sigma T =
    let d1 = d1 S K r sigma T
    let d2 = d1 - sigma * sqrt(T)
    let norm = Normal()
    in
      exp(-r * T) * (S * norm.CumulativeDistribution(d1) - K * norm.CumulativeDistribution(d2)) 

  let put S K r sigma T =
    let d1 = d1 S K r sigma T
    let d2 = d1 - sigma * sqrt(T)
    let norm = Normal()
    in
      exp(-r * T) * (K * norm.CumulativeDistribution(-d2) - S * norm.CumulativeDistribution(-d1)) 

let findIV (target:double) (S:double) (K:double) (r:double) (T:double) =
  let rec NewtonRaphson sigma epsilon =
    let optionPrice = BlackScholes.call S K r sigma T
    let vega = BlackScholes.vega S K r sigma T
    let diff = target - optionPrice
    let _ = printfn "%.2f" (abs diff)
    in
      if abs diff < epsilon then
        sigma
      else
        NewtonRaphson (sigma + diff / vega) epsilon
  let guessSigma = 0.3 // Guess implied volatility of 30%
  let epsilon = 0.001
  in
    NewtonRaphson guessSigma epsilon

module ExcelFunctions =
  
  open ExcelDna.Integration

  [<ExcelFunctionAttribute(Description="Find implied volatility using Newton Raphson.")>]
  let findIV target S K r T =
    findIV target S K r T

// Test
let target = 7.92
let S = (double)1549.28
let K = (double)1620
let r = (double)0.01
let T = (double)(0.1369863013699)

let iv = findIV target S K r T
printfn "%.2f" iv