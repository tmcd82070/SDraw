##Create spatial points object
##Load pre-built dataset
data(meuse)
data(WY)

##Prepare the coordinates
coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)


context("Testing voronoi.polygons()")

test_that("x must be a SpatialPoints* object", {
  # check if the function stops with message
  expect_error(voronoi.polygons(1, 1), "Must pass a SpatialPoints* object to voronoi.polygons.",  fixed=TRUE)
})



# check the output and length
#expect_named(voronoi.polygons(spObj, 5), "sampleID")
#expect_identical((voronoi.polygons(spObj, 10)$sampleID), 1:10)

# check input parameters
#expect_warning(voronoi.polygons(spObj,200), "Sample size is greater than points in the sample frame. 
#                                       n has been reset to the total number of points 
#                                       (i.e., drawing a census).")
#expect_warning(voronoi.polygons(spObj,0), "Sample size less than one has been reset to 1")