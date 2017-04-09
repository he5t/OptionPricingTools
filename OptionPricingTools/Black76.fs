namespace OptionPricingTools

module Black76 =

  open MathNet.Numerics.Distributions

  let norm = Normal ()
  let N = norm.CumulativeDistribution
  let pi = MathNet.Numerics.Constants.Pi

  let d1 S K r sigma T =
    (log(S/K) + (sigma * sigma / 2.0) * T) / (sigma * sqrt(T))

  let d2 S K r sigma T =
    let d1 = d1 S K r sigma T
    d1 - sigma * sqrt(T)

  let vega S K r sigma T =
    let d1 = d1 S K r sigma T
    let n'd1 = 1.0 / sqrt(2.0 * pi) * exp(-d1 * d1 / 2.0)
    S * exp(-r * T) * n'd1 * sqrt(T)

  let call S K r sigma T =
    let d1 = d1 S K r sigma T
    let d2 = d1 - sigma * sqrt(T)
    in
      exp(-r * T) * (S * N(d1) - K * N(d2))

  let put S K r sigma T =
    let d1 = d1 S K r sigma T
    let d2 = d1 - sigma * sqrt(T)
    in
      exp(-r * T) * (K * N(-d2) - S * N(-d1))

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


