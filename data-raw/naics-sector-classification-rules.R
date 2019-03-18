# read
data.naics2sc.rules <- utils::read.csv(
  file = "./data-raw/naics-sector-classification-rules.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)
#save
devtools::use_data(data.naics2sc.rules, overwrite = T)
