# test-extended.gcd.R
context("Testing extended.gcd()")


test_that("begin by inspecting the preliminary error catch", {
  # begin by inspecting the preliminary error catch
  expect_error(extended.gcd( c(3, 6, 9), 5), "'a' and 'b' lengths differ")
})

test_that("ensure that object assignment is inherited from correct object class", {
  # ensure that object assignment is inherited from correct object class
  expect_type(obj <- extended.gcd( c(10, 50, 100, 110), c(9, 9, 9, 9)), "list")
})

test_that("expect that function can handle both numbers, and lists of numbers", {
  # expect that function can handle both numbers, and lists of numbers
  expect_identical((extended.gcd(1, 2)$t), 1)
  expect_identical((extended.gcd( c(1, 2), c(1, 2))$gcd), c(1, 2))
})

test_that("make sure that the list structure that is given as output is formatted correctly", {
  # make sure that the list structure that is given as output is formatted correctly
  expect_equal(ncol(extended.gcd( c(80, 80, 72, 20, 3, 5), c(5, 3, 20, 72, 80, 80))), 5)
  expect_equal(nrow(extended.gcd( c(1500, 87, 55, 77, 0, 1), c(1, 0, 77, 55, 87, 1500))), 6)
})

test_that("force check for negative gcd", {
  # force check for negative gcd
  expect_warning(
    expect((extended.gcd( c(-35, -35), c(-31, -31))$gcd) >= c(0,0), "Failure, negative list elements not handled"),
    "the condition has length > 1 and only the first element will be used", fixed = TRUE)
})