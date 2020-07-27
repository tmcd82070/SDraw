# test-grts.polygon.R
context("Testing grts.polygon()")


# create spatial polygons object
# make "squares" with integer values rounded in a list
square <- rbind( c(2, 4, 3, 4, 3, 5,
                   2, 5, 2, 4, 2, 4),
                 c(6, 9, 7, 9, 7, 8,
                   6, 8, 6, 9, 6, 9))

# give these "squares" an identification
ID <- c("shape1", "shape2")

# create SpatialPolygon object from these squares 
spatPoly <- SpatialPolygons(list(
  Polygons(list(Polygon(matrix(square[1, ], ncol = 2, byrow = TRUE))), ID[1]),
  Polygons(list(Polygon(matrix(square[2, ], ncol = 2, byrow = TRUE))), ID[2])
))


test_that("x must be a SpatialPolygons object", {
  # make sure error catch does it's job
  expect_error(grts.polygon(1, 1, 1), "Must call grts.polygon with a SpatialPolygons object.")
})

test_that("check for output column name", {
  # run SpatialLines object through function, should also improve coverage in grts.equi()
  expect_named(grts.polygon(spatPoly, 4, 1), c("sampleID", "pointType", "geometryID"))
})

test_that("check for output", {
  expect_identical((grts.polygon(spatPoly, 1, 0)$sampleID), "Site-1")
  expect_identical((grts.polygon(spatPoly, 1, 0)$pointType), "Sample")
})