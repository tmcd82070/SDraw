# test-primes.R
context("Testing primes()")


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("primes(15) returns equivalent obj as it did previously", {
  expect_known_value(primes(15), "primes.rds")
})

test_that("errors when too many requested",{
  # check if the function stops with message
  expect_error(primes(1e8+1),"Number of primes requested too large")
})

test_that("negative argument not valid",{
  expect_error(primes(-5), "invalid 'times' argument")
})

test_that("first 5 primes", {
  expect_equal(primes(5), c(2,3,5,7,11))
})

test_that("prime(0) is 2",{
  expect_equal(primes(0), 2)
})
  
test_that("length primes(26) is 26",{
  expect_length(primes(26), length(letters))
})

test_that("decimal input accepted", {
  expect_identical(primes(11.52),primes(11))
  expect_identical(primes(5.971),primes(5.456))
})