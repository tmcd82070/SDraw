##Create spatial polygons object
##Make "squares" with integer values rounded in a list
square <- rbind( c(2, 4, 3, 4, 3, 5,
                   2, 5, 2, 4, 2, 4),
                 c(6, 9, 7, 9, 7, 8,
                   6, 8, 6, 9, 6, 9))
##Give these "squares" an identification
ID <- c("shape1", "shape2")

##Create SpatialPolygon object from these squares 
spatPoly <- SpatialPolygons(list(
  Polygons(list(Polygon(matrix(square[1, ], ncol = 2, byrow = TRUE))), ID[1]),
  Polygons(list(Polygon(matrix(square[2, ], ncol = 2, byrow = TRUE))), ID[2])
))


context("Test the grts.polygon function")

test_that("grts.polygon() operates appropriately", {
  
  ##Make sure error catch does it's job
  expect_error(obj <- grts.polygon(1, 1, 1), "Must call grts.polygon with a SpatialPolygons object.")
  
  ##Run SpatialLines object through function, should also improve coverage in grts.equi()
  expect_named(grts.polygon(spatPoly, 4, 1), c("sampleID",
                                          "pointType",
                                          "geometryID"))
  
  expect_identical((grts.polygon(spatPoly, 1, 0)$sampleID), "Site-1")
  expect_identical((grts.polygon(spatPoly, 1, 0)$pointType), "Sample")
  
})