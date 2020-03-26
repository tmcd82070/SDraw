context("Test the getJ function")

test_that("getJ() operates appropriately", {
  
  ##Inspect initial if statement/error catch
  expect_error(getJ(600, c(3,3,3)), "HIP point currently implemented for 2-dimensional objects only.")
  
  ##Test to see if large values of N are handled
  expect_identical(getJ(63310, c(2,3)), c(8, 6))
  
  ##Test to see if normal values of N are handled
  expect_identical(getJ(3092, c(83, 26)), c(1, 2))
  expect_equal(getJ(4084, c(2, 3)), c(j1=7,j2=3))
  expect_true(is.atomic(getJ(1001, c(2,3))))
  expect_length(getJ(10, c(2, 3)), 2)
})