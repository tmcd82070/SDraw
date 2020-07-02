library(rgeos)
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

##Create a fake dataframe for our new SLDF object
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)


##SpatialLines to SpatialLinesDataFrame
Sldf <- SpatialLinesDataFrame(Sl, data = df)

context("Test the srs.line function")

test_that("srs.line() operates appropriately", {
  
  # check if x is a SpatialLines* object
  expect_error(obj <- srs.line(1,5), "Must call srs.line with a SpatialLines* object.",fixed=TRUE)
  expect_is(srs.line(Sl,10), "SpatialPointsDataFrame")
  expect_is(Sldf,"SpatialLinesDataFrame")
  expect_type(Sldf, "S4")
  
  # check output and length
  expect_equal(length(srs.line(Sl,26)), length(letters))
  expect_length(srs.line(Sldf,5), length(primes(5)))
  
  # check input parameters
  expect_equal(srs.line(Sl,0),NULL)
  expect_type(srs.line(Sl,-3),'NULL')
  
  

})