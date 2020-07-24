# test-polygonArea.R
context("Testing polygonArea()")


# taken from the vignette of the sp package
# create Polygons objects
Sr1  <- Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2  <- Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3  <- Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4  <- Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

# create a single polygons list and a list of polygons
Srs1 <- Polygons(list(Sr1), "s1")
Srs2 <- Polygons(list(Sr2), "s2")
Srs3 <- Polygons(list(Sr1, Sr2), "s1/2")
Srs4 <- Polygons(list(Sr3, Sr4), "s3/4")

# create a SpatialPolygons object
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3,Srs4), 1:4)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
# x is SpatialPolygons
test_that("polygonArea(SpP) returns equivalent obj as it did previously", {
  expect_known_value(polygonArea(SpP), "polygonArea.rds")
})

# a single polygons list
test_that("polygonArea(Srs1) returns equivalent obj as it did previously",{
  expect_known_value(polygonArea(Srs1),"polygonArea_singleList.rds")
})

# a list of polygons
test_that("polygonArea(c(Srs3, Srs4)) returns equivalent obj as it did previously",{
  expect_known_value(polygonArea(c(Srs3, Srs4)),"polygonArea_list.rds")
})