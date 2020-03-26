context("Test the halton.coefficients function")

test_that("halton.coefficients() operates appropriately", {
  
  ##Expect problems when J == 0
  expect_error(halton.coefficients(1, 0))
  
  ##Expect function operates well with large values of samp and J
  expect_equal(dim(halton.coefficients(300:302, c(600,200,100))), c(3,600,3))
  
  ##Expect that object inheritance is strictly part of the 'double' class
  expect_type(obj <- halton.coefficients(10, 20), "double")
  
  ##Check output structure
  expect_identical(as.numeric(halton.coefficients(3,3)), c(1, 1, 0))
})