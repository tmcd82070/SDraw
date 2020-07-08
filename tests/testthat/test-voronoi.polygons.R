##Create spatial points object
##Load pre-built dataset
data(meuse)
data(WY)

##Prepare the coordinates
coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)

WY.samp <- srs.polygon(WY,50)
WY.tess <- voronoi.polygons(WY.samp)


context("Testing voronoi.polygons()")

test_that("x must be a SpatialPoints* object", {
  # check if the function stops with message
  expect_error(voronoi.polygons(1, 1), "Must pass a SpatialPoints* object to voronoi.polygons.",  fixed=TRUE)
})

test_that("x is a SpatialPolygonsDataFrame", {
  expect_is(WY.tess, "SpatialPolygonsDataFrame")
  expect_type(WY.tess, "S4")
})

test_that("check for column name and length", {
  # check the output and length
  expect_named(WY.tess, c("x", "y", "area"))
  expect_length(WY.tess, 50)
})

test_that("range.expand is greater than 2", {
  expect_warning(voronoi.polygons(WY.samp, range.expand= c(2,3,5)), "Only first two elements of range.expand used in voronoi.polygons")
})

test_that("bounding.polygon} is present", {
  expect_is(voronoi.polygons(WY.samp, range.expand=0)@bbox, "matrix")
})
