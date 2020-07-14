# test-hip.point,R
context("Testing hip.point()")


# create spatial points object
# load pre-built dataset
data(meuse)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("hip.point(spObj, 5) returns the equivalent obj as it did previously", {
  expect_known_output(hip.point(spObj, 5), "test-hip.point.rds")
})

# check if the function stops with message
test_that("n must be a SpatialPoints* object", {
  expect_error(obj <- hip.point(1, 1), "Must call hal.point with a SpatialPoints* object.",  fixed=TRUE)
})

test_that("check the output and length",{
  expect_named(hip.point(spObj, 5), "sampleID")
  expect_identical((hip.point(spObj, 10,  plot.lattice = TRUE)$sampleID), 1:10)
})

test_that("n will be assigned to 1 if n is less than 1",{
  expect_warning(hip.point(spObj,0), "Sample size less than one has been reset to 1")
})
