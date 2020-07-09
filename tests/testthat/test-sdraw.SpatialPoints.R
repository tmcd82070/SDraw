##Create spatial points object
##Load pre-built dataset
data(meuse)

##Prepare the coordinates
coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)


context("Testing sdraw.SpatialPoints()")

test_that("invalid SpatialPoint sample type", {
  # check if the function stops with message
  expect_error(sdraw.SpatialPoints(spObj, 15, type="HOP"),"Invalid SpatialPoint sample type = HOP",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 20, type="BTS"),"Invalid SpatialPoint sample type = BTS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 25, type="SAS"),"Invalid SpatialPoint sample type = SAS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 30, type="STS"),"Invalid SpatialPoint sample type = STS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 35, type="RTS"),"Invalid SpatialPoint sample type = RTS",fixed=TRUE )
})

# test_that("length sdraw.SpatialPoints(spObj,13, type = HIP) is 13", {
#   # check the output and length
#   expect_length(sdraw.SpatialPoints(spObj,13, type ="HIP"),aprox(12,13,16))
# })

test_that("length sdraw.SpatialPoints(spObj,26, type = BAS) is 26", {
  # check the output and length
  expect_length(sdraw.SpatialPoints(spObj,26, type ="BAS"),length(LETTERS))
})

test_that("length sdraw.SpatialPoints(spObj,20, type = SSS) is 20", {
  # check the output and length
  expect_length(sdraw.SpatialPoints(spObj,20, type ="SSS"),length(1:20))
})

test_that("length sdraw.SpatialPoints(spObj, 13, type = SRS) is 4", {
  # check the output and length
  expect_length(sdraw.SpatialPoints(spObj,13 , type ="SRS"),aprox(12,13,16))
})

test_that("length sdraw.SpatialPoints(spObj,10, type = GRTS) is 10", {
  # check the output and length
  expect_length(sdraw.SpatialPoints(spObj,10, type ="GRTS"),length(primes(10)))
})