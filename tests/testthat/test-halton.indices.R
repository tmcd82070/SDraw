# test-halton.indices.R
context("Testing halton.indices()")


# create spatial points object
# load pre-built dataset
data(meuse)
data(WA.cities)
data(WY)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)
bb <- bbox(WA.cities) + c(0,0,1,1)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("halton.indices(case_1) returns equivalent obj as it did previously", {
  expect_known_value(halton.indices(WA.cities, J=c(3,2), hl.bbox=bb), "halton.indices_case_1.rds")
})

# x is SpatialPointsDataFrame, missing hl.bbox
test_that("halton.indices() returns equivalent obj as it did previously", {
  expect_known_value(halton.indices(WA.cities, J=c(3,2)), "halton.indices_case_2.rds")
})

# x is SpatialPoints, missing hl.bbox
test_that("halton.indices() returns equivalent obj as it did previously", {
  expect_known_value(halton.indices(spObj, J=c(3,2)), "halton.indices_case_3.rds")
})

# J is null
test_that("halton.indices() returns equivalent obj as it did previously", {
  expect_known_value(halton.indices(WA.cities, hl.bbox=bb), "halton.indices_case_4.rds")
})