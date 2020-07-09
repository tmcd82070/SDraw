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


context("Testing sss.point()")

test_that("x must be a SpatialPoints or data.frame object.", {
  # check if the function stops with message
  expect_error(sss.point(1, 1), "Must call sss.point with a SpatialPoints* or data.frame object.",  fixed=TRUE)
  expect_is(sss.point(spdf, 5), "SpatialPointsDataFrame")
})

test_that("n will be assigned to the number of the rows of the SpatialPoints or data.frame object",{
  # check input parameters
  expect_length(sss.point(spObj,200), 155)
})

test_that("n has to be greater than 0",{
  # check input parameters
  expect_equal(sss.point(spObj,0), NULL)
})

test_that("length sss.point(coords, 15) is 3",{
  # check the output and length
  expect_length(sss.point(coords, 15), length(primes(3)))
})