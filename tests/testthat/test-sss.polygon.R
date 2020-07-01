##Load pre-built dataset
data(WY)


context("Test the sss.polygon function")

test_that("sss.polygon() operates appropriately", {
  
  expect_named(sss.polygon(WY, 30), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
  expect_named(sss.polygon(WY, 25, spacing = c(2,3)), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME")) 
  expect_named(sss.polygon(WY, 20, triangular = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
  expect_named(sss.polygon(WY, 15, rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
  expect_named(sss.polygon(WY, 10, spacing = c(3,4), triangular = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
  expect_named(sss.polygon(WY, 5, triangular = TRUE, rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
  expect_named(sss.polygon(WY, 5, spacing = c(3,5), rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
  expect_named(sss.polygon(WY, 5, spacing = c(2,3),triangular = TRUE, rand.dir = TRUE), c("sampleID", "row", "col", "geometryID", "STATEFP", "COUNTYFP", "NAME"))
  
})