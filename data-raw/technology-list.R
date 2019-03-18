# Technology mix for analysis
data.technology <-
  c(
    "Electric",
    "Hybrid",
    "ICE",
    "GasCap",
    "CoalCap",
    "OilCap",
    "RenewablesCap",
    "HydroCap",
    "NuclearCap",
    "Coal",
    "Oil",
    "Gas"
  )
devtools::use_data(data.technology, overwrite = T)
