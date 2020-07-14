# test-voronoi.polygons.R
context("Testing voronoi.polygons()")


# create spatial points object
# load pre-built dataset
data(meuse)
data(WY)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)
WY.samp <- srs.polygon(WY,50)
WY.tess <- voronoi.polygons(WY.samp)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("voronoi.polygons(WY.samp) returns the equivalent obj as it did previously", {
  expect_known_output(voronoi.polygons(WY.samp), "voronoi.polygons.rds")
})

# check if the function stops with message
test_that("x must be a SpatialPoints* object", {
  expect_error(voronoi.polygons(1, 1), "Must pass a SpatialPoints* object to voronoi.polygons.",  fixed=TRUE)
})

test_that("x is a SpatialPolygonsDataFrame", {
  expect_is(WY.tess, "SpatialPolygonsDataFrame")
  expect_type(WY.tess, "S4")
})

test_that("range.expand is greater than 2", {
  expect_warning(voronoi.polygons(WY.samp, range.expand= c(2,3,5)), "Only first two elements of range.expand used in voronoi.polygons")
})

# check the output and length
test_that("check for column name and length", {
  expect_named(WY.tess, c("x", "y", "area"))
  expect_length(WY.tess, 50)
})

test_that("bounding.polygon is present", {
  expect_is(voronoi.polygons(WY.samp, range.expand=0)@bbox, "matrix")
})