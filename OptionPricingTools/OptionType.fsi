namespace OptionPricingTools

module OptionType =

  type t =
    | Call
    | Put

  val ofString : string -> t


