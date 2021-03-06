# test-getJ.R
context("Testing getJ()")


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("getJ(72, c(2,3)) returns equivalent obj as it did previously", {
  expect_known_value(getJ(72, c(2,3)), "getJ.rds")
})

# Inspect initial if statement/error catch
test_that("error when it's not 2-dimensional objects", {
  expect_error(getJ(600, c(3,3,3)), "HIP point currently implemented for 2-dimensional objects only.")
})

# Test to see if normal values of N are handled
test_that("check for N less than 62208",{
  expect_identical(getJ(3092, c(83, 26)), c(1, 2))
  expect_equal(getJ(4084, c(2, 3)), c(j1=7,j2=3))
  expect_equal(getJ(72, c(2,3)), c(j1=3, j2=2))
  expect_true(is.atomic(getJ(1001, c(2,3))))
})

# Test to see if normal values of N are handled
test_that("length getJ(10, c(2, 3)) is 2",{
  expect_length(getJ(10, c(2, 3)), 2)
})

# Test to see if large values of N are handled
test_that("check for bigger N",{
  expect_identical(getJ(63310, c(2,3)), c(8, 6))
  expect_identical(getJ(165432, c(3,5)), c(6, 4))
})