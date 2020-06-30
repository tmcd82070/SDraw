##Create spatial points object
##Load pre-built dataset
data(meuse)

##Prepare the coordinates

coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)


context("Test the hip.point function")


test_that("hip.point() operates appropriately", {
  
  # check if the function stops with message
  expect_error(obj <- hip.point(1, 1), "Must call hal.point with a SpatialPoints* object.",  fixed=TRUE)
  
  # check the output and length
  expect_named(hip.point(spObj, 5), "sampleID")
  expect_identical((hip.point(spObj, 10)$sampleID), 1:10)
  
  # check input parameters
  # expect_warning(hip.point(spObj,200), "Sample size is greater than points in the sample frame. 
  #                                       n has been reset to the total number of points 
  #                                       (i.e., drawing a census).")
  expect_warning(hip.point(spObj,0), "Sample size less than one has been reset to 1")
  
})