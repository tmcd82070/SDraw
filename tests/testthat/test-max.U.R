# test-maxU.R
context("Testing max.U()")


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("maxU() returns the equivalent obj as it did previously", {
  expect_known_value(maxU(), "maxU.rds")
})

# Expect to return 10e7 or 100,000,000
test_that("maxU() return 10e7 or 100,000,000", {
  expect_equal(maxU(), 10e7)
  expect_identical(maxU(), 10e7)
})