##Load pre-built dataset
data(WY)


context("Testing bas.polygon()")

test_that("x is a SpatialPointsDataFrame", {
  expect_is(bas.polygon(WY,25), "SpatialPointsDataFrame")
  expect_type(bas.polygon(WY, 20), "S4")
})

test_that("check for the column names",{
  expect_named(bas.polygon(WY, 50), c("sampleID", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
})

test_that("warning when sample size less than 1",{
  expect_warning(bas.polygon(WY, 0), "Sample size less than one has been reset to 1")
})
