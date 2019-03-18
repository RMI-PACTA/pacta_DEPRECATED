# read
file.benchmark.regions.table <- utils::read.csv(
  file = "./data-raw/benchmark-regions.csv", 
  header = T, stringsAsFactors = F, na.strings = "")
# translate data frame into list of vectors
data.benchmark.regions <- Map(f = function(x) {x[!is.na(x)]}, file.benchmark.regions.table)
# save
devtools::use_data(data.benchmark.regions, overwrite = T)
