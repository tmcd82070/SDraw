context("Test the max.U function")

test_that("maxU() operates appropriately", {
  
  # Expect to return 10e7 or 100,000,000
  expect_equal(maxU(), 10e7)
  expect_identical(maxU(), 10e7)

})