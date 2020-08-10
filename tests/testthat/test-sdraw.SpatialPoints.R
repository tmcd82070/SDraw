# test-sdraw.SpatialPoints.R
context("Testing sdraw.SpatialPoints()")


# create spatial points object
# load pre-built dataset
data(meuse)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)


# check if the function stops with message
test_that("invalid SpatialPoint sample type", {
  capture.output(expect_error(sdraw.SpatialPoints(spObj, 15, type="HOP"),"Invalid SpatialPoint sample type = HOP",fixed=TRUE))
  capture.output(expect_error(sdraw.SpatialPoints(spObj, 20, type="BTS"),"Invalid SpatialPoint sample type = BTS",fixed=TRUE))
  capture.output(expect_error(sdraw.SpatialPoints(spObj, 25, type="SAS"),"Invalid SpatialPoint sample type = SAS",fixed=TRUE))
  capture.output(expect_error(sdraw.SpatialPoints(spObj, 30, type="STS"),"Invalid SpatialPoint sample type = STS",fixed=TRUE))
  capture.output(expect_error(sdraw.SpatialPoints(spObj, 35, type="RTS"),"Invalid SpatialPoint sample type = RTS",fixed=TRUE))
})

# check the output and length
test_that("length sdraw.SpatialPoints(spObj,26, type = BAS) is 26", {
  capture.output(expect_length(sdraw.SpatialPoints(spObj, 26, type ="BAS"),length(LETTERS)))
})

# check the output and length
test_that("length sdraw.SpatialPoints(spObj,20, type = SSS) is 20", {
  capture.output(expect_length(sdraw.SpatialPoints(spObj, 20, type ="SSS"),length(1:20)))
})

# check the output and length
test_that("length sdraw.SpatialPoints(spObj,10, type = GRTS) is 10", {
  capture.output(expect_length(sdraw.SpatialPoints(spObj, 10, type ="GRTS"),length(primes(10))))
})