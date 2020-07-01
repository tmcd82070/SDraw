##Create spatial polygons object
v =  sample(1:10,24,replace = TRUE)
dim(v) = c(3,2,4)


context("Test the hip.lattice.polygon function")

test_that("hip.lattice.polygon() operates appropriately", {
  
  # check if the function stops with message
  expect_error(hip.lattice.polygon(box = v, getJ(72, bases = c(2,3)), bases = c(2,3)), "incorrect number of dimensions")
  

  
})


