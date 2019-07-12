#' Prepare test loanbook based on list of names, industry codes, countries
#' 
#' @return Loanbook data-frame with minimum required columnset
#' 
#' @export
#' 
#' @param name Company name
#' @param country Residense country
#' @param industry Company NAICS code
#' @param industry_desc Company NAICS description
#' @param rows Number of loans to Generate. In case number is provided companies 
#'   will be randomly picked up using \code{sample} function with 
#'   \code{replace = T} property.
#' @param seed Seed for rundom generation
process.generateTestLoanbook <- function(
  name, country, 
  industry, industry_desc = NULL, 
  rows = NULL, seed = NULL
) {
  seed <- if (is.null(seed)) 1000 else seed
  industry_desc <- if (is.null(industry_desc)) "" else industry_desc

  dataset <- data.frame(
    Company.Name = name,
    NAICS = industry,
    Country = country,
    NAICS.Description = industry_desc
  )

  data <- 
    if (is.null(rows)) dataset 
    else {
      smp <- sample(x = nrow(dataset), size = rows, replace = T)
      dataset[smp,]
    }

  n <- nrow(data)

  test_loanbook <- data.frame(
    Source = "pacta::process.generateTestLoanbook()",
    Reporting.Date = Sys.Date(),
    Facility.ID = paste("Facility", substr(0.1E10 + 1:n, 3, 10), sep = "-"),
    Outstanding = {set.seed(seed); rpois(n, 1e3) * 1e3},
    Exposure    = {set.seed(seed); rpois(n, 1e3) * 1e3},
    Loan.Currency = "USD",
    Maturity.Date = Sys.Date() + as.integer({set.seed(seed+1000); rpois(n, 52)} * 7 * 3),

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

  test_loanbook

}
