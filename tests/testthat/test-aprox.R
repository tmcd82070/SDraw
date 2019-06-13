context("Test the aprox function")

test_that("aprox() operates appropriately", {
  
  x<- rnorm(20)
  y<- rnorm(10)
  x.out<- sort(runif(2))
  ##NA is to be expected, basic test for junk data
  expect_output(aprox(x,y,x.out), NA)
  
  ##Variable integer inputs, to ensure that 
  ##the basics of the function are working
  expect_identical(aprox(1,2,3), 2)
  expect_identical(aprox(20,20,20), 20)
  expect_identical(aprox(0,0,0), 0)
  expect_identical(aprox(-2,6,-4), 6)
  

  ##A final test series that just checks for proper vector creation/output
  expect_equal(aprox(c(1,2,3), c(4,5,9,9), c(1,2,3)), c(4, 5, 9))
  expect_equal(aprox(c(1,2,3), 2, c(1,2,3)), c(2, NA, 2))
  expect_equal(aprox(c(1,2,3), c(20,22,24,26,28), c(1,2,3,4,5,6)), c(20, 22, 28, 28, 28, 28))
})
