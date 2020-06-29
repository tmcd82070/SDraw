context("Test the primes function")

test_that("primes() operates appropriately", {

  expect_equal(primes(5), c(2,3,5,7,11))

  expect_length(primes(26), length(letters))

  expect_error(primes(1e8+1),"Number of primes requested too large")


})