# test-halton.R
context("Testing halton()")


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("halton(10, 2) returns equivalent obj as it did previously", {
  expect_known_value(halton(10, 2), "halton.rds")
})

test_that("length dim equals to length primes(dim)",{
  expect_length(halton(n=1,dim= 101), length(primes(101)))
})

test_that("length start not equals to dim", {
  expect_error(halton(1, dim= 10, start = c(2,3)), "The start vector must either have length 1 or length equal to the number of dimensions")
})

test_that("return matrix(0,1,dim)", {
  expect_equal(halton(1,dim= 2,start= 0), matrix(0,1,2))
  expect_identical(halton(1,dim= 3,start= 0), matrix(0,1,3))
})