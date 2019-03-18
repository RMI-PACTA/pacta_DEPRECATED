# read
data.country.iso.codes <- utils::read.csv(
  file = "./data-raw/country-iso-codes.csv", 
  colClasses = "character", header = T, stringsAsFactors = F, na.strings = "")
#save
devtools::use_data(data.country.iso.codes, overwrite = T)
