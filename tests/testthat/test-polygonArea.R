# test-polygonArea.R
context("Testing polygonArea()")


# load pre-built dataset
data(WY)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("polygonArea(WY) returns equivalent obj as it did previously", {
  expect_known_value(polygonArea(WY), "polygonArea.rds")
})