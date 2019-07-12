library(pacta)

names <- c("Coumpany A", "Coumpany B")
industries <- c("Ind 1", "Ind 2")
countries <- c("Country A", "Country B")

test_that("generate loans using only names and industries (and default country)", {
  lbk <- process.generateTestLoanbook(name = names, industry = industries, country = "Country A")
  expect_equal(nrow(lbk), length(names))
})

test_that("number of loans equal to number of company names", {
  lbk <- process.generateTestLoanbook(name = names, industry = industries, country = countries)
  expect_equal(nrow(lbk), length(names))
})

test_that("number of loans equal to number of company names", {
  lbk <- process.generateTestLoanbook(name = names, industry = industries, country = countries)
  expect_equal(nrow(lbk), length(names))
})

test_that("number of loans equal to value of rows parameter", {
  rows <- 999
  lbk <- process.generateTestLoanbook(name = names, industry = industries, country = countries, rows = rows)
  expect_equal(nrow(lbk), rows)
})

test_that("for the same given seed result is the same", {
  seed = 1000
  rows <- 999
  lbk1 <- process.generateTestLoanbook(name = names, industry = industries, country = countries, rows = rows, seed = seed)
  lbk2 <- process.generateTestLoanbook(name = names, industry = industries, country = countries, rows = rows, seed = seed)
  expect_equal(lbk1, lbk2)
})

test_that("for the different seeds result is different", {
  rows <- 999
  lbk1 <- process.generateTestLoanbook(name = names, industry = industries, country = countries, rows = rows, seed = 1)
  lbk2 <- process.generateTestLoanbook(name = names, industry = industries, country = countries, rows = rows, seed = 2)
  expect_failure(expect_equal(lbk1, lbk2))
})
