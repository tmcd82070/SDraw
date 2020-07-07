##Load pre-built dataset
data(WY)

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


context("Testing srs.polygon()")

test_that("x is a SpatialPointsDataFrame", {
  expect_is(srs.polygon(WY,25), "SpatialPointsDataFrame")
  expect_type(srs.polygon(WY, 20), "S4")
})

test_that("check for the column names",{
  expect_named(srs.polygon(WY, 50), c("sampleID", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("warning when sample size less than 1",{
  expect_warning(srs.polygon(WY, 0), "Sample size less than one has been reset to 1")
})