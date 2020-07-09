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


context("Testing sss.line()")

test_that("x must be a a SpatialLines object", {
  # check if x is a SpatialLines* object
  expect_error(obj <- sss.line(1,1), "Must call sss.line with a SpatialLines* object.",fixed=TRUE)
  expect_is(sss.line(Sl,10), "SpatialPointsDataFrame")
  expect_is(Sldf,"SpatialLinesDataFrame")
  expect_type(Sldf, "S4")
})

test_that("both n and spacing specified needed ", {
  # check if n or spacing is missing with message
  expect_warning(sss.line(Sl,10,10),"n and spacing both specified in sss.line.  n is being used.")
})

test_that("n must be an integer and greater than 0", {
  # check input parameters
  expect_equal(sss.line(Sl,0),NULL)
  expect_type(sss.line(Sl,-3),'NULL')
})

test_that("length sss.line(Sldf,10, random.start = FALSE) is 10", {
  # check input parameters
  expect_length(sss.line(Sldf,10, random.start = FALSE), length(primes(10)))
})