##Create spatial points object
##Load pre-built dataset
data(meuse)

##Prepare the coordinates

coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)


context("Test the grts.point function")

test_that("grts.point() operates appropriately", {
  
  ##Make sure error catch does it's job
  expect_error(obj <- grts.point(1, 1, 1), "Must call grts.line with a SpatialPoints object.")
  
  ##Run SpatialLines object through function, should also improve coverage in grts.equi()
  expect_named(grts.point(spObj, 4, 1), c("sampleID",
                                      "pointType",
                                      "geometryID"))
  expect_identical((grts.point(spObj, 1, 0)$sampleID), "Site-1")
  expect_identical((grts.point(spObj, 1, 0)$pointType), "Sample")
  
})