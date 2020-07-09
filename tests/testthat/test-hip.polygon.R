##Load pre-built dataset
data(WY)


context("Testing hip.polygon()")

test_that("error when dimensions is greater than 2", {
  expect_error(hip.polygon(WY, 5, bases=c(2,3,4), J=c(8,5)), "HIP polygon sampling not implemented for dimensions greater
         than 2.")
})

test_that("warning when sample is 0",{
  expect_warning(hip.polygon(WY, 0, bases=c(2,3), J=c(3,2)), "Sample size less than one has been reset to 1")
})

test_that("error when x missing",{
  expect_error(hip.polygon(NULL, 10, bases=c(2,3), J=c(8,5)),"Need to specify spatial points data.frame.")
})

test_that("error when bases not 2-dimensional",{
  expect_error(hip.polygon(WY, 10, bases=2, J=c(8,5)), "Bases must be 2-dimensional, i.e. c(2,3)", fixed= TRUE)
})