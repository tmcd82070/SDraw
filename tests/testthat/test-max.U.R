context("Test the max.U function")

test_that("maxU() operates appropriately", {

  expect_equal(maxU(), 10e7)

  expect_identical(maxU(), 10e7)



})