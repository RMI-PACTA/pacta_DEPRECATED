# Sector Classification list
data.sector.classification <- 
  c(
    "Automotive",
    "Aviation",
    "Cement",
    "Fossil Fuels",
    "Power", 
    "Shipping",
    "Steel"
  )
devtools::use_data(data.sector.classification, pkg = "twodii4loans", overwrite = T)
