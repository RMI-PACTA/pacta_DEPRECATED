# read
file.index.regions <- utils::read.csv(
  file = "./data-raw/index-regions.csv", 
  header = T, stringsAsFactors = F, na.strings = "")
# translate data frame into list of vectors
data.index.regions <- Map(f = function(x) {x[!is.na(x)]}, file.index.regions)
# save
devtools::use_data(data.index.regions, overwrite = T)
