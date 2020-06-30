##Create spatial points object
##Load pre-built dataset
data(meuse)

##Prepare the coordinates

coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)


context("Test the srs.point function")


test_that("srs.point() operates appropriately", {
  
  # check if the function stops with message
  expect_error(obj <- srs.point(1, 1), "Must call srs.point with a SpatialPoints* or data.frame object.",  fixed=TRUE)
  
  # check the output and length
  expect_named(srs.point(spObj, 5), c("sampleID","geometryID"))
  expect_identical((srs.point(spObj, 10)$sampleID), 1:10)
  
  # check input parameters
  expect_length(srs.point(spObj,200), 155)
  expect_equal(srs.point(spObj,0), NULL)
  
})