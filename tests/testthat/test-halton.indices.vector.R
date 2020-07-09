df <- data.frame(x=(0:100)/101, y = 0.2)

context("Testing halton.indices.vector()")

test_that("halton.indices.vector(df, c(16, 9)) has 101 integers", {
  expect_length(halton.indices.vector(df, c(16, 9)), 101)
  expect_type(halton.indices.vector(df, c(16, 9)), "integer")
})

test_that("check halton.indices.vector(df, c(16, 9)) output", {
  ##Verify that output structure is the same as what is given in the original example
  expect_equal(halton.indices.vector(df, c(16,9))[1:7], c(48, 48, 48, 48, 48, 48, 48))
  expect_equal(halton.indices.vector(df, c(16,9))[80:85], c(3, 3, 3, 3, 75, 75))
})