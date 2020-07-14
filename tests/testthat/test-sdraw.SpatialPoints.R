# test-sdraw.SpatialPoints.R
context("Testing sdraw.SpatialPoints()")


# create spatial points object
# load pre-built dataset
data(meuse)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("sdraw.SpatialPoints() returns equivalent obj as it did previously", {
  expect_known_value(sdraw.SpatialPoints(spObj, 15, type="BAS"), "sdraw.SpatialPoints.rds")
})

# check if the function stops with message
test_that("invalid SpatialPoint sample type", {
  expect_error(sdraw.SpatialPoints(spObj, 15, type="HOP"),"Invalid SpatialPoint sample type = HOP",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 20, type="BTS"),"Invalid SpatialPoint sample type = BTS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 25, type="SAS"),"Invalid SpatialPoint sample type = SAS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 30, type="STS"),"Invalid SpatialPoint sample type = STS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 35, type="RTS"),"Invalid SpatialPoint sample type = RTS",fixed=TRUE )
})

# check the output and length
test_that("length sdraw.SpatialPoints(spObj,26, type = BAS) is 26", {
  expect_length(sdraw.SpatialPoints(spObj,26, type ="BAS"),length(LETTERS))
})

# check the output and length
test_that("length sdraw.SpatialPoints(spObj,20, type = SSS) is 20", {
  expect_length(sdraw.SpatialPoints(spObj,20, type ="SSS"),length(1:20))
})

# check the output and length
test_that("length sdraw.SpatialPoints(spObj, 13, type = SRS) is 4", {
  expect_length(sdraw.SpatialPoints(spObj,13 , type ="SRS"),aprox(12,13,16))
})

# check the output and length
test_that("length sdraw.SpatialPoints(spObj,10, type = GRTS) is 10", {
  expect_length(sdraw.SpatialPoints(spObj,10, type ="GRTS"),length(primes(10)))
})