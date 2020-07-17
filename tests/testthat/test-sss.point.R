# test-sss.point.R
context("Testing sss.point()")


# create spatial points object
# load pre-built dataset
data(meuse)

# prepare the 3 components: coordinates, data, and proj4string
coords <- meuse[ , c("x", "y")]   # coordinates
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# assign
spObj <- SpatialPoints(coords)

# make the spatial points data frame object
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = data, 
                               proj4string = crs)


# check if the function stops with message
test_that("x must be a SpatialPoints or data.frame object.", {
  expect_error(sss.point(1, 1), "Must call sss.point with a SpatialPoints* or data.frame object.",  fixed=TRUE)
  expect_is(sss.point(spdf, 5), "SpatialPointsDataFrame")
})

# check input parameters
test_that("n will be assigned to the number of the rows of the SpatialPoints or data.frame object",{
  expect_length(sss.point(spObj,200), 155)
})

# check input parameters
test_that("n has to be greater than 0",{
  expect_equal(sss.point(spObj,0), NULL)
})

# check the output and length
test_that("length sss.point(coords, 15) is 3",{
  expect_length(sss.point(coords, 15), length(primes(3)))
})