library(pacta)

test_that("import pipe operator", {
  add2 <- function(x) {x + 2}
  expect_equal(2 %>% add2, 4)  
})
