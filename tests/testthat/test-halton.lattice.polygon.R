# test-halton.lattice.polygon.R
context("Testing halton.lattice.polygon()")


# load dataset
data(WY)

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


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
# x is not SpatialPolygonsDataFrame
test_that("halton.lattice.polygon(spatPoly) returns equivalent obj as it did previously", {
  expect_known_value(halton.lattice.polygon(spatPoly), "halton.lattice.polygon_notSPDF.rds")
})