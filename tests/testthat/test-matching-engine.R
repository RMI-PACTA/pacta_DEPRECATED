library(twodii4loans)

test_that("number of results", {
  expect_equal(nrow(matchNames(1, 0, 0, "all")), 1)
  expect_equal(nrow(matchNames(1, 0, 1, "all")), 0)

  expect_equal(nrow(matchNames(1:10, 1:10, 0, "all")), 10*10)
  expect_equal(nrow(matchNames(1:10, 1:10, 0, "closest")), 10)
  expect_equal(nrow(matchNames(1:10, 1:10, 1)), 10)
})

test_that("number of columns in dataframe", {
  expect_equal(dim(matchNames(1, 1:10, 0, "all")), c(10, 3))
  expect_equal(dim(matchNames(1, 1:10, 0, "closest")), c(1, 3))
  expect_equal(dim(matchNames(1, 2:9, 1)), c(0, 3))
})

test_that("score not lower than threshold", {
  t <- .5
  m <- matchNames(1:123, 1:123, t)$Score
  expect_equal(sum(m[m < t], na.rm = T), 0)
})

test_that("matching by blocks - number of columns", {
  m00 <- list(n = 0:4, bn = replicate(5, "a1"), d = 5:9, bd = replicate(5, "b1"))
  m10 <- list(n = 0:7, bn = replicate(8, "a2"), d = 5:9, bd = replicate(5, "b2"))
  m01 <- list(n = 0:4, bn = replicate(5, "a3"), d = 5:9, bd = replicate(5, "a3"))
  m11 <- list(n = 0:7, bn = replicate(8, "a4"), d = 5:9, bd = replicate(5, "a4"))
  expect_equal(ncol(matchNamesByBlocks(m00$n, m00$bn, m00$d, m00$bd)), 4)
  expect_equal(ncol(matchNamesByBlocks(m01$n, m01$bn, m10$d, m10$bd)), 4)
  expect_equal(ncol(matchNamesByBlocks(m10$n, m10$bn, m01$d, m01$bd)), 4)
  expect_equal(ncol(matchNamesByBlocks(m11$n, m11$bn, m11$d, m11$bd)), 4)
  expect_equal(ncol(matchNamesByBlocks("A", "A", NA, NA)), 4)
  
})

test_that("matching by blocks - number of rows", {
  m00 <- list(n = 0:4, bn = replicate(5, "a1"), d = 5:9, bd = replicate(5, "b1"))
  m10 <- list(n = 0:7, bn = replicate(8, "a2"), d = 5:9, bd = replicate(5, "b2"))
  m01 <- list(n = 0:4, bn = replicate(5, "a3"), d = 5:9, bd = replicate(5, "a3"))
  m11 <- list(n = 0:7, bn = replicate(8, "a4"), d = 5:9, bd = replicate(5, "a4"))
  expect_equal(nrow(matchNamesByBlocks("A", "A", NA, NA)), 0)
  expect_equal(nrow(matchNamesByBlocks("A", "", NA, NA)), 0)
  expect_equal(nrow(matchNamesByBlocks(m00$n, m00$bn, m00$d, m00$bd)), 0)
  expect_equal(nrow(matchNamesByBlocks(m01$n, m01$bn, m10$d, m10$bd)), 0)
  expect_equal(nrow(matchNamesByBlocks(m10$n, m10$bn, m01$d, m01$bd)), 0)
  expect_equal(nrow(matchNamesByBlocks(m11$n, m11$bn, m11$d, m11$bd)), 3)
  expect_equal(
    nrow(matchNamesByBlocks(
      names = c(m00$n, m10$n, m01$n, m11$n), 
      nblocks = c(m00$bn, m10$bn, m01$bn, m11$bn), 
      dictionary = c(m00$d, m10$d, m01$d, m11$d),
      dblocks = c(m00$bd, m10$bd, m01$bd, m11$bd),
      return = "all"
    )), 
    3
  )
  expect_equal(
    nrow(matchNamesByBlocks(
      names      = c(m00$n, m10$n, m01$n, m11$n), 
      nblocks    = c(m00$bn, m10$bn, m01$bn, m11$bn), 
      dictionary = c(m00$d, m10$d, m01$d, m11$d),
      dblocks    = c(m00$bd, m10$bd, m01$bd, m11$bd),
      threshold = 0,
      return = "all"
    )), 
    65
  )
})

test_that("mathing engine returns scores equal to stringSim", {
  
  # values for testing (comparison to be performed: n[i] vs d[i])
  n1 <- c("ca" , "ABC", "abc", "ab" , "abc")
  d1 <- c("abc", "abc", "ca" , "abc", "cba")
  
  n2 <- c("hello", "ab", "survey" , "MARTHA", "Euler", "Euler" )
  d2 <- c("HeLl0", "ba", "surgery", "MATHRA", "Euler", "Ellery")
  
  #functions to present matching scores (similarity)
  stringsim_fun <- function(n, d) {
    mapply(stringSim, n, d, SIMPLIFY = T, USE.NAMES = F)
  }
  matchname_fun <- function(n, d) {
    fun <- function(n, d) matchNames(n[1], d[1], 0)$Score
    mapply(
      FUN = fun, n, d, SIMPLIFY = T, USE.NAMES = F
      )
  }
  expect_equal(stringsim_fun(n1, d1), matchname_fun(d1, n1))
  expect_equal(stringsim_fun(n2, d2), matchname_fun(d2, n2))
  
})
