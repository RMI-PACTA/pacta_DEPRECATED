# read
data.name.reductions <- utils::read.csv(
  file = "./twodii4loans/data-raw/name-reductions.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)
#save
devtools::use_data(data.name.reductions, pkg = "twodii4loans", overwrite = T)
