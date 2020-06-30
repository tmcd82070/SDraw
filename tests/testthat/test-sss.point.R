##Create spatial points object
##Load pre-built dataset
data(meuse)

##Prepare the coordinates

coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)


context("Test the sss.point function")


test_that("sss.point() operates appropriately", {
  
  ##Make sure error catch does it's job
  expect_error(obj <- sss.point(1, 1), "Must call sss.point with a SpatialPoints* or data.frame object.",  fixed=TRUE)
  
  #Run SpatialPoints object through function
  expect_named(sss.point(spObj, 5), c("sampleID","geometryID"))

  expect_identical((sss.point(spObj, 10)$sampleID), 1:10)
  expect_length(sss.point(spObj,200), 155)
  expect_equal(sss.point(spObj,0), NULL)
  
})