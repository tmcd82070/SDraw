# test-bas.point.R
context("Testing bas.point()")


# load pre-built dataset
data(meuse)

# prepare the 3 components: coordinates, data, and proj4string
coords <- meuse[ , c("x", "y")]   # coordinates
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# make the spatial points data frame object
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = data, 
                               proj4string = crs)

# assign
testSamp<-bas.point(spdf,20)
spdfDuplicate <- rbind(spdf, spdf)


test_that("Throws error on non-SpatialPoints imput", {  
  expect_error(bas.point(8,50), "Must call bas.point with a SpatialPoints* object.", fixed=TRUE)
})

test_that("returns SpatialPointsDataFrame", {
  expect_is(testSamp, "SpatialPointsDataFrame")
})

test_that("sample size is greater than frame size",{
  expect_warning(bas.point(spdf, 200), "Sample size greater than frame size. Census taken.")
})

test_that("error trap: check for duplicate coordinates", {
  expect_error(bas.point(spdfDuplicate, 10), "Minimum distance between points is zero: are there duplicate coordinates?")
})

test_that("return contains sampleID", {
  expect_equal(testSamp$sampleID, rep(1:20))
})

test_that("return contains geometryID", {
  expect_true("geometryID" %in% names(testSamp))
})

test_that("returns correct number of columns", {
  expect_equal(ncol(testSamp), 14)
})

test_that("returns correct number of rows", {
  expect_equal(nrow(testSamp), 20)
})

test_that("attributes are returned", {
  expect_true("zinc" %in% names(testSamp))
  expect_true("lead" %in% names(testSamp))
})

test_that("no empty header values", {
  expect_false(" " %in% names(testSamp))
})