##Create spatial points object
##Load pre-built dataset
data(meuse)

# prepare the 3 components: coordinates, data, and proj4string

coords <- meuse[ , c("x", "y")]   # coordinates
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

##Assign
spObj <- SpatialPoints(coords)

# make the spatial points data frame object
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = data, 
                               proj4string = crs)

context("Test the srs.point function")


test_that("srs.point() operates appropriately", {
  
  # check if the function stops with message
  expect_error(srs.point(1, 1), "Must call srs.point with a SpatialPoints* or data.frame object.",  fixed=TRUE)
  
  # check the output and length
  expect_length(srs.point(coords, 15), length(primes(3)))
  expect_is(srs.point(spdf, 5), "SpatialPointsDataFrame")
  
  # check input parameters
  expect_length(srs.point(spObj,200), 155)
  expect_equal(srs.point(spObj,0), NULL)
  
})