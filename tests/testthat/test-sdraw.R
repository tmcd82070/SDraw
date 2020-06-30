data(WY)



context("Test the sdraw function")

test_that("sdraw() operates appropriately", {

  # check the output and length
  expect_length(basSample <- sdraw(WY, 10, type="BAS"), length(1:10))
  expect_length(grtsSample <- sdraw(WY, 26, type="GRTS"), length(LETTERS))
  expect_length(randomSample <- sdraw(WY, 26, type="SRS"), length(letters))
  expect_length(haltonSample <- sdraw(WY, 5, type="HIP"), length(primes(5)))
  
})