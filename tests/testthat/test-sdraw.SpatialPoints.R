##Create spatial points object
##Load pre-built dataset
data(meuse)

##Prepare the coordinates

coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)


context("Test the sdraw.SpatialPoints function")

test_that("sdraw.SpatialPoints() operates appropriately", {
  
  expect_error(sdraw.SpatialPoints(spObj, 15, type="HOP"),"Invalid SpatialPoint sample type = HOP",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 20, type="BTS"),"Invalid SpatialPoint sample type = BTS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 25, type="SAS"),"Invalid SpatialPoint sample type = SAS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 30, type="STS"),"Invalid SpatialPoint sample type = STS",fixed=TRUE )
  expect_error(sdraw.SpatialPoints(spObj, 35, type="RTS"),"Invalid SpatialPoint sample type = RTS",fixed=TRUE )
  
  expect_length(sdraw.SpatialPoints(spObj,13, type ="HIP"),aprox(12,13,16))
  expect_length(sdraw.SpatialPoints(spObj,26, type ="BAS"),length(LETTERS))
  expect_length(sdraw.SpatialPoints(spObj,20, type ="SSS"),length(1:20))
  expect_length(sdraw.SpatialPoints(spObj,04, type ="SRS"),length(c(2,3,5,7)))
  expect_length(sdraw.SpatialPoints(spObj,10, type ="GRTS"),length(primes(10)))
  
})


