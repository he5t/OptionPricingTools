namespace OptionPricingTools

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

  let findIV (target:double) (optType:OptionType.t) (S:double) (K:double) (r:double) (T:double) =
    let rec NewtonRaphson iter maxIter sigma epsilon =
      let optionPrice =
        match optType with
        | OptionType.Call -> call S K r sigma T
        | OptionType.Put -> put S K r sigma T
      let vega = vega S K r sigma T
      let diff = target - optionPrice
      let _ = printfn "%.2f" (abs diff)
      in
        if abs diff < epsilon then
          sigma
        elif iter > maxIter then
          failwith "Cannot converge"
        else
          NewtonRaphson (iter + 1) maxIter (sigma + diff / vega) epsilon
    let guessSigma = 0.3 // Guess implied volatility of 30%
    let epsilon = 0.001
    in
      NewtonRaphson 0 100 guessSigma epsilon


