# read
data.file <- utils::read.csv(
  file = "./data-raw/test-loanbook.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)

#generate
data.test.loanbook <- process.generateTestLoanbook(
  name = data.file$Company.Name,
  country = data.file$Country,
  industry = data.file$NAICS,
  industry_desc = data.file$NAICS.Description, 
  seed = 1000)

data.test.loanbook$Source <- "pacta::data.test.loanbook"

#save
devtools::use_data(data.test.loanbook, overwrite = T)
