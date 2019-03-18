# read
data.dataset.req.files <- utils::read.csv(
  file = "./twodii4loans/data-raw/dataset-req-files.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)
#save
devtools::use_data(data.dataset.req.files, pkg = "twodii4loans", overwrite = T)
