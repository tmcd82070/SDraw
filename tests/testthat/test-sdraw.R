data(WY)


context("Test the sdraw function")

test_that("sdraw() operates appropriately", {

  expect_length(randomSample <- sdraw(WY, 26, type="SRS"), length(LETTERS))
  expect_length(basSample <- sdraw(WY, 26, type="BAS"), length(LETTERS))
  expect_length(grtsSample <- sdraw(WY, 26, type="GRTS"), length(LETTERS))
  expect_length(haltonSample <- sdraw(WY, 5, type="HIP"), length(primes(5)))
})