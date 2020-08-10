# test-hip.point,R
context("Testing hip.point()")


# create spatial points object
# load pre-built dataset
data(meuse)
data(WA.cities)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)
spObj2x <- SpatialPoints(rbind(coords, coords))
spObj6 <- spObj[1:6,]

# check if the function stops with message
test_that("n must be a SpatialPoints* object", {
  expect_error(obj <- hip.point(1, 1), "Must call hal.point with a SpatialPoints* object.",  fixed=TRUE)
})

test_that("check the output and length",{
  expect_named(hip.point(spObj, 5), "sampleID")
  expect_identical((hip.point(spObj2x, 10, J=c(4,2), plot.lattice = TRUE)$sampleID), 1:10)
})

test_that("n will be assigned to 1 if n is less than 1",{
  expect_warning(hip.point(spObj,0), "Sample size less than one has been reset to 1")
})

test_that("n is greater than length of sample frame", {
  expect_warning(hip.point(spObj6,10), "Sample size is greater than points in the sample frame. 
            n has been reset to the total number of points (i.e., drawing a census)." ,fixed=TRUE)
})

test_that("J results in more Halton boxes than there are points", {
  expect_warning(hip.point(spObj, 15, J=c(3,5)), "J results in more Halton boxes than there are points. J has been set to default value.", fixed=TRUE)
})

test_that("reset maxSampleSize", {
  bases <- NULL
  N <- NULL
  J <- NULL
  maxSampleSize <- NULL
  expect_warning(hip.point(WA.cities, 500, plot.lattice = TRUE), c("Sample size is greater than max sample size for HIP sampling.
  HIP sampling discards some points while drawing Halton lattice.
  n has been set to ", maxSampleSize, ", the largest possible sample for N = ", N, " and J = (", J[1], ",", J[2], ")"),fixed=TRUE)
})