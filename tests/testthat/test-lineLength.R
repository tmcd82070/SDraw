# test-lineLength.R
context("Testing lineLength()")


# create spatial points object
# load pre-built dataset
data(meuse)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)

# create some arbitrary lines
l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))

# creat some formal class lines with these lines
Sl1 <- Line(l1)
Sl2 <- Line(l2)

# assign arbitrary ID's to these formal class lines
S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")

# create a formal class SpatialLines object
Sl <- SpatialLines(list(S1,S2))

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

# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("lineLength(spObj) returns equivalent obj as it did previously", {
  expect_known_value(lineLength(spObj), "lineLength_points.rds")
})

test_that("lineLength(Sl) returns equivalent obj as it did previously", {
  expect_known_value(lineLength(Sl), "lineLength_lines.rds")
})

test_that("lineLength(spatPoly) returns equivalent obj as it did previously", {
  expect_known_value(lineLength(spatPoly), "lineLength_polygons.rds")
})

test_that("x is a SpatialPoints", {
  expect_equal(lineLength(spObj), 0)
})

test_that("lineLength(Sl) is 4.650282",{
  expect_equal(round(lineLength(Sl),6), 4.650282)
})

test_that("lineLength(spatPoly) is 8",{
  expect_equal(lineLength(spatPoly), length(primes(8)))
})
