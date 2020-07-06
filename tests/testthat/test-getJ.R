context("Testing getJ()")

test_that("error when it's not 2-dimensional objects", {
  # Inspect initial if statement/error catch
  expect_error(getJ(600, c(3,3,3)), "HIP point currently implemented for 2-dimensional objects only.")
})

test_that("check for N less than 62208",{
  # Test to see if normal values of N are handled
  expect_identical(getJ(3092, c(83, 26)), c(1, 2))
  expect_equal(getJ(4084, c(2, 3)), c(j1=7,j2=3))
  expect_equal(getJ(72, c(2,3)), c(j1=3, j2=2))
  expect_true(is.atomic(getJ(1001, c(2,3))))
})

test_that("check for bigger N",{
  # Test to see if large values of N are handled
  expect_identical(getJ(63310, c(2,3)), c(8, 6))
  expect_identical(getJ(165432, c(3,5)), c(6, 4))
})

test_that("length getJ(10, c(2, 3)) is 2",{
  # Test to see if normal values of N are handled
  expect_length(getJ(10, c(2, 3)), 2)
})