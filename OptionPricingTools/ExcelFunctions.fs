namespace OptionPricingTools

module ExcelFunctions =

  open ExcelDna.Integration

  [<ExcelFunction(Description="Find implied volatility using Newton Raphson.")>]
  let FINDIMPLIEDVOL 
    (
      [<ExcelArgumentAttribute(Name="Target", Description="Option price")>] target,
      [<ExcelArgumentAttribute(Name="Type", Description="Option type 'C' or 'P'")>] optType,
      [<ExcelArgumentAttribute(Name="Underlying", Description="Underlying price")>] S,
      [<ExcelArgumentAttribute(Name="Strike", Description="Strike price")>] K,
      [<ExcelArgumentAttribute(Name="Risk-free rate", Description="Risk free rate")>] r,
      [<ExcelArgumentAttribute(Name="Time (years)", Description="Time to expiry in years")>] T 
    ) =
      if System.Double.IsNaN(target) then
        raise (System.ArgumentException())
      if System.String.IsNullOrEmpty(optType) || optType <> "C" && optType <> "P" then
        raise (System.ArgumentException())
      elif System.Double.IsNaN(S) then
        raise (System.ArgumentException())
      elif System.Double.IsNaN(K) then
        raise (System.ArgumentException())
      elif System.Double.IsNaN(r) then
        raise (System.ArgumentException())
      elif System.Double.IsNaN(T) then
        raise (System.ArgumentException())
      else
        BlackScholes.findIV target optType S K r T