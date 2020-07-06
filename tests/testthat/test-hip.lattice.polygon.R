##Create spatial polygons object
v =  sample(1:10,24,replace = TRUE)
dim(v) = c(3,2,4)


context("Testing hip.lattice.polygon()")

test_that("dimension is greater than 2", {
  # check if the function stops with message
  expect_error(hip.lattice.polygon(box = v, getJ(72, bases = c(2,3)), bases = c(2,3)), "incorrect number of dimensions")
})


