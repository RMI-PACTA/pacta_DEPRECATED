# read
data.dataset.req.attrs <- utils::read.csv(
  file = "./data-raw/dataset-req-attrs.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)
#save
devtools::use_data(data.dataset.req.attrs, overwrite = T)
