# read
data.name.reductions <- utils::read.csv(
  file = "./data-raw/name-reductions.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)
#save
devtools::use_data(data.name.reductions, overwrite = T)
