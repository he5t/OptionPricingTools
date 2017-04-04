namespace OptionPricingTools

module OptionType =

  type t =
    | Call
    | Put

  let ofString (s:string) =
    if s = "C" then
      Call
    elif s = "P" then
      Put
    else
      failwithf "Input string must be either C for call, or P for put options, but it was %s" s

