data(WY)


context("Test the hip.polygon function")

test_that("hip.polygon() operates appropriately", {
  
  expect_error(hip.polygon(WY, 50, bases=c(2,3,4), J=c(8,5)), "HIP polygon sampling not implemented for dimensions greater
         than 2.")
  expect_error(hip.polygon(NULL, 100, bases=c(2,3), J=c(8,5)),"Need to specify spatial points data.frame.")
  expect_error(hip.polygon(WY, 50, bases=2, J=c(8,5)), "Bases must be 2-dimensional, i.e. c(2,3)", fixed= TRUE)
  
  expect_warning(hip.polygon(WY, 0, bases=c(2,3), J=c(3,2)), "Sample size less than one has been reset to 1")
  expect_warning(hip.polygon(WY, -10, bases=c(2,3), J=c(3,2)), "Sample size less than one has been reset to 1")
  
  
})