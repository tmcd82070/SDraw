context("Test the primes function")

test_that("primes() operates appropriately", {
  
  # check the output and length
  expect_equal(primes(5), c(2,3,5,7,11))
  expect_equal(primes(0), 2)
  expect_length(primes(26), length(letters))
  expect_identical(primes(11.52),primes(11))
  expect_identical(primes(5.971),primes(5.456))
  
  # check if the function stops with message
  expect_error(primes(1e8+1),"Number of primes requested too large")
  expect_error(primes(-5), "invalid 'times' argument")


})