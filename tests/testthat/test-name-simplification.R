library(twodii4loans)

test_that("test is_empty for return value", {
  expect_true(is.na(simplifyName(NA)))
  expect_equal(simplifyName(""), "")
  expect_equal(simplifyName("A"), "a")
  expect_equal(simplifyName(" A and B "), "ab")
  expect_equal(simplifyName("public limited company"), "plc")
})

test_that("simplify vector", {
  expect_equal(simplifyName(c("A", "B")), c("a", "b"))
})

test_that("replacements", {
  expect_equal(
    simplifyName(c(" and ", " och ", " en ", " und ", " & ")), 
    c("", "", "", "", "")
  )
  expect_equal(
    simplifyName(c(" . ", " , ", " - ", " / ", " $ ")), 
    c("", "", "", "", "")
  )
})

test_that("ownership cut-out", {
  expect_equal(simplifyName("One-Two-Three plc"), "onetwothree plc")
  expect_equal(simplifyName("One-Two-Three plc", cut.ownership = T), "onetwothree")
})

test_that("custom reduction rules", {
  red <- data.frame(From = "AAAA", To = "BBB", stringsAsFactors = F)
  expect_equal(simplifyName("Aaa Aaaa", F, red), "aaabbb")
  expect_equal(simplifyName("AAA and AAA", F, red), "aaaandaaa")
})

test_that("custom ownership types", {
  red <- data.frame(From = character(), To = character())
  own <- c("a1", "a2")
  expect_equal(simplifyName("Test a1", F, red, own), "test a1")
  expect_equal(simplifyName("Test a1 a3", T, red, own), "testa1a3")
})
