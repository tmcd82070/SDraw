library(rgeos)
##Taken from the vignette of the sp package:

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


context("Test the sss.line function")

test_that("sss.line() operates appropriately", {
  
  # check if x is a SpatialLines* object
  expect_error(obj <- sss.line(1,1), "Must call sss.line with a SpatialLines* object.",fixed=TRUE)
  expect_is(sss.line(Sl,10), "SpatialPointsDataFrame")
  expect_is(Sldf,"SpatialLinesDataFrame")
  expect_type(Sldf, "S4")
  
  #check if n or spacing is missing with message
  expect_warning(sss.line(Sl,10,10),"n and spacing both specified in sss.line.  n is being used.")
  
  ## check input parameters
  expect_equal(sss.line(Sl,0),NULL)
  expect_length(sss.line(Sldf,10, random.start = FALSE), length(primes(10)))
  
})