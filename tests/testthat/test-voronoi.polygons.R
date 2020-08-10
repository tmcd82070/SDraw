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

# set.seed(101)
# pts <- SpatialPoints(cbind(runif(1000), runif(1000)))
# smp <- pts[sample(1:length(pts), 10),]
# bound.pts <- cbind(c(0.2503111693,  0.5215198166,  0.8074680642,
#                      0.9312807075,  0.9047494268,  0.7750409433,
#                      0.3033737308,  0.0000000000,  0.0321650835,
#                      0.0321650835),
#                    c(0.03098592, 0.14595480, 0.03688176,
#                      0.25502784, 0.89472650, 1.00000000,
#                      0.80334098, 0.52918441, 0.14005896,
#                      0.14005896))
# bounding.poly <- SpatialPolygons(list(Polygons(list(Polygon(bound.pts)), "b")), as.integer(1))
# vor <- SDraw::voronoi.polygons(smp, bounding.poly)