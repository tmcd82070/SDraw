##Create spatial lines object

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

context("Test the grts.line function")

test_that("grts.line() operates appropriately", {
  
  ##Make sure error catch does it's job
  expect_error(obj <- grts.line(1, 1, 1), "Must call grts.line with a SpatialLines object.")
  
  ##Run SpatialLines object through function, should also improve coverage in grts.equi()
  expect_named(grts.line(Sl, 4, 1), c("sampleID",
                                      "pointType",
                                      "geometryID"))
  expect_identical((grts.line(Sl, 1, 0)$sampleID), "Site-1")
  expect_identical((grts.line(Sl, 1, 0)$pointType), "Sample")
  
})