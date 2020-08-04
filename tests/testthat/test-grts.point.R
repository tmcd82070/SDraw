# test-grts.point.R
context("Testing grts.point()")


# create spatial points object
# load pre-built dataset
data(meuse)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)


test_that("x must be a SpatialPoints object", {
  # make sure error catch does it's job
  expect_error(grts.point(1, 1, 1), "Must call grts.point with a SpatialPoints object.")
})

test_that("check for output column name", {
  # run SpatialLines object through function, should also improve coverage in grts.equi()
  expect_named(grts.point(spObj, 4, 1), c("sampleID", "pointType", "geometryID"))
})

test_that("check for output", {
  expect_identical((grts.point(spObj, 1, 0)$sampleID), "Site-1")
  expect_identical((grts.point(spObj, 1, 0)$pointType), "Sample")
})