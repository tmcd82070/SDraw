# test-bas.line.R
context("Testing bas.line()")


# load required package
library(rgeos)

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

# create a fake dataframe for our new SLDF object
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)

# spatialLines to SpatialLinesDataFrame
Sldf <- SpatialLinesDataFrame(Sl, data = df)

# assign 
testSamp <- bas.line(Sldf,10)


# ensure that this function will not accept anything other than a SLDF object
test_that("x must be a SpatialLinesDataFrame object", {
  expect_error(bas.line(2,2), "Must call bas.line with a SpatialLines* object.", fixed=TRUE)
})

# this is a basic test to ensure that testSamp object has been assigned 
# data correctly from the baseline function
test_that("x is a SpatialLines* object", {
  expect_is(testSamp, "SpatialPointsDataFrame")
})

test_that("x is a 2-dimensional BAS sample", {
  expect_type(bas.line(Sl, 10, balance = "2D"), "S4")
  expect_is(bas.line(Sl, 5, balance = "2D"), "SpatialPointsDataFrame")
})

# these tests look at some individual facets of the base line function
# this is to ensure that the baseline function has operated as it should
test_that("length bas.line(Sldf,10) is 10", {
  expect_equal(length(bas.line(Sldf,10)$df), 10)
  expect_equal(bas.line(Sldf,10)$sampleID, rep(1:10))
})

test_that("length bas.line(Sl,10) is 10", {
  expect_equal(length(bas.line(Sl,10)$geometryID), 10)
  expect_equal(bas.line(Sl,10)$sampleID, rep(1:10))
})

# an error is expected, as this should be a null object assignment
test_that("error when x is NULL", {
  emptySearch <- testSamp
  expect_error(as.SpatialLines.SLDF(emptySearch), "no slot of name \"lines\" for this object of class \"SpatialPointsDataFrame\"")
})