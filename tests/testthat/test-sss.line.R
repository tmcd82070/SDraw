# test-sss.line.R
context("Testing sss.line()")


# load required package
library(rgeos)

# this section of code serves the purpose of creating a "mock"
# SLDF in an effort to properly test the baseline function in a stand-alone
# manner.

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


# check if x is a SpatialLines* object
test_that("x must be a a SpatialLines object", {
  expect_error(obj <- sss.line(1,1), "Must call sss.line with a SpatialLines* object.",fixed=TRUE)
  expect_is(sss.line(Sl,10), "SpatialPointsDataFrame")
  expect_is(Sldf,"SpatialLinesDataFrame")
  expect_type(Sldf, "S4")
})

# check if n or spacing is missing with message
test_that("both n and spacing specified needed ", {
  expect_warning(sss.line(Sl,10,10),"n and spacing both specified in sss.line.  n is being used.")
})

# check input parameters
test_that("n must be an integer and greater than 0", {
  expect_equal(sss.line(Sl,0),NULL)
  expect_type(sss.line(Sl,-3),'NULL')
})

# check input parameters
test_that("length sss.line(Sldf,10, random.start = FALSE) is 10", {
  expect_length(sss.line(Sldf,10, random.start = FALSE), length(primes(10)))
})