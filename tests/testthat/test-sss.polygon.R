# test-sss.polygon.R
context("Testing sss.polygon()")


# load pre-built dataset
data(WY)

# create spatial polygons object
# make "squares" with integer values rounded in a list
square <- rbind( c(2, 4, 3, 4, 3, 5,
                   2, 5, 2, 4, 2, 4),
                 c(6, 9, 7, 9, 7, 8,
                   6, 8, 6, 9, 6, 9))

# give these "squares" an identification
ID <- c("shape1", "shape2")

# create SpatialPolygon object from these squares 
spatPoly <- SpatialPolygons(list(
  Polygons(list(Polygon(matrix(square[1, ], ncol = 2, byrow = TRUE))), ID[1]),
  Polygons(list(Polygon(matrix(square[2, ], ncol = 2, byrow = TRUE))), ID[2])
))


test_that("check if x is SpatialPolygons", {
  expect_is(sss.polygon(spatPoly,15), "SpatialPointsDataFrame")
  expect_type(sss.polygon(spatPoly, 15), "S4")
})

test_that("check for the column names when none specified", {
  expect_named(sss.polygon(spatPoly, 15), c("sampleID", "row", "col", "geometryID", "ID"))
})

test_that("check for the column names when none specified", {
  expect_named(sss.polygon(WY, 30), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("check for the column names when only spacing specified", {
  expect_named(sss.polygon(WY, 25, spacing = c(2,3)), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("check for the column names when only triangular specified", {
  expect_named(sss.polygon(WY, 20, triangular = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("check for the column names when only rand.dir specified", {
  expect_named(sss.polygon(WY, 15, rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("check for the column names when both spacing and triangular specified", {
  expect_named(sss.polygon(WY, 10, spacing = c(3,4), triangular = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("check for the column names when both triangular and rand.dir specified", {
  expect_named(sss.polygon(WY, 5, triangular = TRUE, rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("check for the column names when both spacing and rand.dir specified",{
  expect_named(sss.polygon(WY, 5, spacing = c(3,5), rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
}) 

test_that("check for the column names when all specified", {
  expect_named(sss.polygon(WY, 5, spacing = c(2,3),triangular = TRUE, rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("check for the column names when rand.dir user-specified", {
  expect_warning(sss.polygon(WY, 15, rand.dir = c(3,5)), "the condition has length > 1 and only the first element will be used")
})