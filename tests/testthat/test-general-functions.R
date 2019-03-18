library(twodii4loans)

test_that("test is_empty", {
  expect_equal(is_empty(NA), T)
  expect_equal(is_empty(""), T)
  expect_equal(is_empty(11), F)
  expect_equal(is_empty(T), F)
  expect_equal(is_empty(c(T, F)), c(F, F))
  expect_equal(is_empty(1:10), replicate(10, FALSE))
  expect_equal(is_empty(c("A", NA, "B", "", "C")), c(F, T, F, T, F))
})

test_that("test trim", {
  expect_equal(trim(""), "")
  expect_equal(trim("A"), "A")
  expect_equal(trim("A "), "A")
  expect_equal(trim(" A"), "A")
  expect_equal(trim(" A "), "A")
  expect_equal(trim(" A B "), "A B")
  expect_equal(trim(c(" A ", " B ")), c("A", "B"))
})

test_that("dup.test", {
  expect_equal(dup.test(NA), FALSE)
  expect_equal(dup.test(1), FALSE)
  expect_equal(dup.test(c(1, 1)), c(T, T))
  expect_equal(dup.test(c(1, NA)), c(F, F))
  expect_equal(dup.test(c(NA, NA)), c(T, T))
  expect_equal(dup.test(c(1, 0, 1)), c(T, F, T))
  expect_equal(dup.test(1:100), replicate(100, F))
  expect_equal(dup.test(c(1:100, 100:1)), replicate(200, T))
  expect_equal(
    dup.test(c(1:150, 101:250)), 
    c(replicate(100, F), replicate(100, T), replicate(100, F))
  )
})

test_that("convert NAICS to Sector Classification", {
  level = c("Sector", "Subsector", "Industry", "Sector")
  code = c("11", "112", "112233", "22")
  rules <- data.frame(
    Level = level,
    Code  = code,
    Sector.Classification = mapply(
      FUN = function(x, y) {paste(x, y, sep = "-")},
      level, code
    ),
    stringsAsFactors = F
  )
  expect_equal(convert.naics2sc("112233", rules), "Industry-112233")
  expect_equal(convert.naics2sc("112200", rules), "Subsector-112")
  expect_equal(convert.naics2sc("110000", rules), "Sector-11")
  expect_true(is.na(convert.naics2sc("120000", rules)))
  expect_equal(convert.naics2sc("223344", rules), "Sector-22")
  # test vector
  expect_equal(
    convert.naics2sc(c("111111", "112200", "112233", "220000", "330000"), rules),
    c("Sector-11", "Subsector-112", "Industry-112233", "Sector-22", NA)
  )
})

test_that("convert NAICS to Sector Classification - ambiguity", {
  rules <- data.frame(
    Level = c("Sector", "Sector"),
    Code = c("11", "11"),
    Sector.Classification = c("Class1", "Class2"),
    stringsAsFactors = F
  )
  expect_equal(
    convert.naics2sc("112200", rules), 
    "Class1"
  )
  expect_equal(
    convert.naics2sc(c("112200", "112233", "220000"), rules), 
    c("Class1", "Class1", NA)
  )
})

test_that("convert NAICS to Sector Classification - order preservation", {
  rules <- data.frame(
    Level = c("Sector", "Sector", "Sector"),
    Code = c("10", "11", "12"),
    Sector.Classification = c("C0", "C1", "C2"),
    stringsAsFactors = F
  )
  expect_equal(
    convert.naics2sc(
      c("999999", "12xxxx", "11xxxx", "10xxxx"), 
      rules
    ), 
    c(  NA      , "C2"    , "C1"    , "C0"    )
  )
})