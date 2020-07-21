# test-halton.indices.CRT.R
context("Testing halton.indices.CRT()")


# create mock data frame
df <- data.frame(x=(0:100)/101, y = 0.2)
pt <- data.frame(x=0.43, y=0.64)
n.boxes <- c(16,9)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("halton.indices.CRT(pt, n.boxes) returns equivalent obj as it did previously", {
  expect_known_value(halton.indices.CRT(pt, n.boxes), "halton.indices.CRT.rds")
})

test_that("halton.indices.CRT() operates appropriately", {
  # check that error catch for J = exponents operates as it should
  expect_error(halton.indices.CRT(df, c(1, 2), "number of boxes in one or more dimensions is not an integer power of bases. Check n.boxes and b."))
})

test_that("halton.indices.CRT(df, c(16, 9)) has 101 doubles", {
  # make sure that n.boxes only accepts vector arguments
  expect_error(halton.indices.CRT(df, 16), "number of boxes in one or more dimensions is not an integer power of bases. Check n.boxes and b.")
  expect_length(halton.indices.CRT(df, c(16, 9)), 101)
  expect_type(halton.indices.CRT(df, c(16, 9)), "double")
})

test_that("check halton.indices.CRT(df, c(16, 9)) output", {
  # verify that output structure is the same as what is given in the original example
  expect_identical(halton.indices.CRT(df, c(16,9))[1:7], c(48, 48, 48, 48, 48, 48, 48))
  expect_identical(halton.indices.CRT(df, c(16,9))[80:85], c(3, 3, 3, 3, 75, 75))
})
