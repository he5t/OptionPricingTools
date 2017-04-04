namespace OptionPricingTools

module BlackScholes =

   val findIV : target:float -> optionType:OptionType.t -> S:float -> K:float -> r:float -> T:float -> double