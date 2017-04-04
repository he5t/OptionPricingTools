module ExcelFunctions

  open ExcelDna.Integration

  [<ExcelFunctionAttribute(Description="Find implied volatility using Newton Raphson.")>]
  let findIV target S K r T =
    BlackScholes.findIV target S K r T

