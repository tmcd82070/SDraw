context("Test the extended.gcd function")


test_that("extended.gcd() operates appropriately", {
  
  ##Begin by inspecting the preliminary error catch
  expect_error(extended.gcd( c(3, 6, 9), 5), "'a' and 'b' lengths differ")
  
  ##Ensure that object assignment is inherited from correct object class
  expect_type(obj <- extended.gcd( c(10, 50, 100, 110), c(9, 9, 9, 9)), "list")
  
  ##Expect that function can handle both numbers, and lists of numbers
  expect_identical((extended.gcd(1, 2)$t), 1)
  expect_identical((extended.gcd( c(1, 2), c(1, 2))$gcd), c(1, 2))
  
  ##Make sure that the list structure that is given as output is formatted correctly
  expect_equal(ncol(extended.gcd( c(80, 80, 72, 20, 3, 5), c(5, 3, 20, 72, 80, 80))), 5)
  expect_equal(nrow(extended.gcd( c(1500, 87, 55, 77, 0, 1), c(1, 0, 77, 55, 87, 1500))), 6)
 
  ##Force check for negative gcd
  expect((extended.gcd( c(-35, -35), c(-31, -31))$gcd) >= c(0,0), "Failure, negative list elements not handled")
  
})
