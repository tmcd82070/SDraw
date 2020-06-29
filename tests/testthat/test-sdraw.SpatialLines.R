##Taken from the vignette of the sp package:

##This section of code serves the purpose of creating a "mock"
##SLDF in an effort to properly test the baseline function in a stand-alone
##manner.

##Create some arbitrary lines
l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))

##Creat some formal class lines with these lines
Sl1 <- Line(l1)
Sl2 <- Line(l2)

##Assign arbitrary ID's to these formal class lines
S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")

##Create a formal class SpatialLines object
Sl <- SpatialLines(list(S1,S2))


context("Test the sdraw.SpatialLines function")

test_that("sdraw.SpatialLines() operates appropriately", {
  
  expect_error(sdraw.SpatialLines(Sl, 10, type="BTS"),"Invalid SpatialLines sample type = BTS",fixed=TRUE )
  expect_error(sdraw.SpatialLines(Sl, 10, type="SAS"),"Invalid SpatialLines sample type = SAS",fixed=TRUE )
  expect_error(sdraw.SpatialLines(Sl, 10, type="STS"),"Invalid SpatialLines sample type = STS",fixed=TRUE )
  expect_error(sdraw.SpatialLines(Sl, 10, type="AGRTS"),"Invalid SpatialLines sample type = AGRTS",fixed=TRUE )
  
  expect_length(sdraw.SpatialLines(Sl,26, type ="BAS"),length(LETTERS))
  expect_length(sdraw.SpatialLines(Sl,10, type ="SSS"),length(1:10))
  expect_length(sdraw.SpatialLines(Sl,4, type ="SRS"),length(c(2,3,5,7)))
  expect_length(sdraw.SpatialLines(Sl,10, type ="GRTS"),length(primes(10)))
  
})


