# read
data.file <- utils::read.csv(
  file = "./twodii4loans/data-raw/test-loanbook.csv", 
  colClasses = "character", header = T, stringsAsFactors = F)

#generate columns
rows <- nrow(data.file)

data.test.loanbook <- data.frame(
  Source = "twodii4loans::data.test.loanbook",
  Reporting.Date = Sys.Date(),
  Facility.ID = paste("Facility", substr(0.1E10 + 1:10, 3, 10), sep = "-"),
  Outstanding = {set.seed(1001); rpois(rows, 1e3) * 1e3},
  Exposure    = {set.seed(1001); rpois(rows, 1e3) * 1e3},
  Loan.Currency = "USD",
  Maturity.Date = Sys.Date() + as.integer({set.seed = 2001; rpois(rows, 52)} * 7 * 3),

  Asset.ID = "",
  Asset.Name = "",
  Borrower.ID = data.file$Company.Name,
  Borrower.Name = data.file$Company.Name,
  Borrower.Country = data.file$Country,
  Borrower.NAICS = data.file$NAICS,
  Borrower.NAICS.Description = data.file$NAICS.Description,

  Sector.Classification = twodii4loans::convert.naics2sc(data.file$NAICS),

  Ultimate.Parent.ID = data.file$Company.Name,
  Ultimate.Parent.Name = data.file$Company.Name,
  Ultimate.Parent.Country = data.file$Country,
  Ultimate.Parent.NAICS = data.file$NAICS,
  Ultimate.Parent.NAICS.Description = data.file$NAICS.Description
)
#save
devtools::use_data(data.test.loanbook, pkg = "twodii4loans", overwrite = T)
