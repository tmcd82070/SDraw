# test-sss.polygon.R
context("Testing sss.polygon()")


# load pre-built dataset
data(WY)


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