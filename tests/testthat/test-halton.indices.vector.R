# test-halton.indices.vector.R
context("Testing halton.indices.vector()")


# create the dataframe
df <- data.frame(x=(0:100)/101, y = 0.2)
pt <- data.frame(x=0.43, y=0.64)
n.boxes <- c(16,9)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("halton.indices.vector(pt, n.boxes) returns equivalent obj as it did previously", {
  expect_known_value(halton.indices.vector(pt, n.boxes), "halton.indices.vector.rds")
})

test_that("halton.indices.vector(df, c(16, 9)) has 101 integers", {
  expect_length(halton.indices.vector(df, c(16, 9)), 101)
  expect_type(halton.indices.vector(df, c(16, 9)), "integer")
})

# verify that output structure is the same as what is given in the original example
test_that("check halton.indices.vector(df, c(16, 9)) output", {
  expect_equal(halton.indices.vector(df, c(16,9))[1:7], c(48, 48, 48, 48, 48, 48, 48))
  expect_equal(halton.indices.vector(df, c(16,9))[80:85], c(3, 3, 3, 3, 75, 75))
})