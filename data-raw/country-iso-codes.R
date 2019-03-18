# read
data.country.iso.codes <- utils::read.csv(
  file = "./twodii4loans/data-raw/country-iso-codes.csv", 
  colClasses = "character", header = T, stringsAsFactors = F, na.strings = "")
#save
devtools::use_data(data.country.iso.codes, pkg = "twodii4loans", overwrite = T)
