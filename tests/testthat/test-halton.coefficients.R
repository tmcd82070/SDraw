# test-halton.coefficients.R
context("Testing halton.coefficients()")


test_that("error when J == 0", {
  # expect problems when J == 0
  expect_error(halton.coefficients(1, 0))
})

test_that("function operates well with large values of samp and J",{
  # expect function operates well with large values of samp and J
  expect_equal(dim(halton.coefficients(300:302, c(600,200,100))), c(3,600,3))
})

test_that("object inheritance is strictly part of the 'double' class",{
  # expect that object inheritance is strictly part of the 'double' class
  expect_type(obj <- halton.coefficients(10, 20), "double")
})

test_that("check output structure",{
  # check output structure
  expect_identical(as.numeric(halton.coefficients(3,3)), c(1, 1, 0))
})