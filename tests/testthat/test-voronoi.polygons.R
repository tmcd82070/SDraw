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


#check if the function stops with message
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

# # check the output and length
test_that("check for column name and length", {
  expect_named(WY.tess, c("x", "y", "area"))
  expect_length(WY.tess, 50)
})