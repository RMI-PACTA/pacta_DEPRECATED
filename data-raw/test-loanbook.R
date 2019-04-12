# read
data.file <- utils::read.csv(
  file = "./data-raw/test-loanbook.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)

#generate columns
seed <- 1001
rows <- 1000

data <- data.file[sample(x = nrow(data.file), size = rows, replace = T),]

data.test.loanbook <- data.frame(
  Source = "pacta::data.test.loanbook",
  Reporting.Date = Sys.Date(),
  Facility.ID = paste("Facility", substr(0.1E10 + 1:rows, 3, 10), sep = "-"),
  Outstanding = {set.seed(seed); rpois(rows, 1e3) * 1e3},
  Exposure    = {set.seed(seed); rpois(rows, 1e3) * 1e3},
  Loan.Currency = "USD",
  Maturity.Date = Sys.Date() + as.integer({set.seed(seed+1000); rpois(rows, 52)} * 7 * 3),

  Asset.ID = "",
  Asset.Name = "",
  Borrower.ID = data$Company.Name,
  Borrower.Name = data$Company.Name,
  Borrower.Country = data$Country,
  Borrower.NAICS = data$NAICS,
  Borrower.NAICS.Description = data$NAICS.Description,

  Sector.Classification = pacta::convert.naics2sc(data$NAICS),

  Ultimate.Parent.ID = data$Company.Name,
  Ultimate.Parent.Name = data$Company.Name,
  Ultimate.Parent.Country = data$Country,
  Ultimate.Parent.NAICS = data$NAICS,
  Ultimate.Parent.NAICS.Description = data$NAICS.Description
)
#save
devtools::use_data(data.test.loanbook, overwrite = T)
