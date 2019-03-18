# read
data.dataset.req.files <- utils::read.csv(
  file = "./data-raw/dataset-req-files.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)
#save
devtools::use_data(data.dataset.req.files, overwrite = T)
