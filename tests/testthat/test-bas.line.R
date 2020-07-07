library(rgeos)
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

##Create a fake dataframe for our new SLDF object
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)

##SpatialLines to SpatialLinesDataFrame
Sldf <- SpatialLinesDataFrame(Sl, data = df)

testSamp<-bas.line(Sldf,10)


context("Testing bas.line()")

test_that("x is a SpatialLines* object", {
  # This is a basic test to ensure that testSamp object has been assigned 
  # data correctly from the baseline function
  expect_is(testSamp, "SpatialPointsDataFrame")
})

test_that("x must be a SpatialLinesDataFrame object", {
  # To ensure that this function will not accept anything other than a SLDF object
  expect_error(bas.line(2,2), "Must call bas.line with a SpatialLines* object.", fixed=TRUE)
})

test_that("length bas.line(Sldf,10) is 10", {
  # These tests look at some individual facets of the base line function
  # This is to ensure that the baseline function has operated as it should
  expect_equal(length(bas.line(Sldf,10)$df), 10)
  expect_equal(bas.line(Sldf,10)$sampleID, rep(1:10))
})

test_that("length bas.line(Sl,10) is 10", {
  expect_equal(length(bas.line(Sl,10)$geometryID), 10)
  expect_equal(bas.line(Sl,10)$sampleID, rep(1:10))
})

test_that("error when x is NULL", {
  # An error is expected, as this should be a null object assignment
  emptySearch <- testSamp
  expect_error(as.SpatialLines.SLDF(emptySearch), "no slot of name \"lines\" for this object of class \"SpatialPointsDataFrame\"")
})

test_that("x is a 2-dimensional BAS sample", {
  expect_type(bas.line(Sl,10, balance = "2D"), "S4")
  expect_is(bas.line(Sl,5, balance = "2D"), "SpatialPointsDataFrame")
})
