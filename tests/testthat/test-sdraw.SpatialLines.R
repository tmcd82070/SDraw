# test-sdraw.SpatialLines.R
context("Testing sdraw.SpatialLines()")


# create some arbitrary lines
l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))

# creat some formal class lines with these lines
Sl1 <- Line(l1)
Sl2 <- Line(l2)

# assign arbitrary ID's to these formal class lines
S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")

# create a formal class SpatialLines object
Sl <- SpatialLines(list(S1,S2))


# check if the function stops with message
test_that("invalid SpatialLines sample type", {
  capture.output(expect_error(sdraw.SpatialLines(Sl, 10, type="BTS"),"Invalid SpatialLines sample type = BTS",fixed=TRUE))
  capture.output(expect_error(sdraw.SpatialLines(Sl, 10, type="SAS"),"Invalid SpatialLines sample type = SAS",fixed=TRUE))
  capture.output(expect_error(sdraw.SpatialLines(Sl, 10, type="STS"),"Invalid SpatialLines sample type = STS",fixed=TRUE))
  capture.output(expect_error(sdraw.SpatialLines(Sl, 10, type="AGRTS"),"Invalid SpatialLines sample type = AGRTS",fixed=TRUE))
})

# check the output and length
test_that("length sdraw.SpatialLines(Sl,26, type ='BAS') is 26", {
  capture.output(expect_length(sdraw.SpatialLines(Sl,26, type ="BAS"),length(LETTERS)))
})

# check the output and length
test_that("length sdraw.SpatialLines(Sl,10, type ='SSS' is 10", {
  capture.output(expect_length(sdraw.SpatialLines(Sl,10, type ="SSS"),length(1:10)))
})

# check the output and length
test_that("length sdraw.SpatialLines(Sl,26, type ='SRS') is 4", {
  capture.output(expect_length(sdraw.SpatialLines(Sl,4, type ="SRS"),length(c(2,3,5,7))))
})

# check the output and length
test_that("length sdraw.SpatialLines(Sl,10, type ='GRTS') is 10", {
  capture.output(expect_length(sdraw.SpatialLines(Sl,10, type ="GRTS"),length(primes(10))))
})