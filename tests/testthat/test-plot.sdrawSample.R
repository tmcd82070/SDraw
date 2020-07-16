# test-plot.sdrawSample.R
context("Testing plotSample()")


# create spatial points object
# load pre-built dataset
data(meuse)
data(WY)

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

# assign
HAL <- halton.lattice.polygon(WY, J=c(3,2), eta=c(3,2), triangular=TRUE )


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("plotSample() returns equivalent obj as it did previously", {
  expect_known_value(plotSample(sdraw(HAL, 25, type="SRS"), WY, lattice=TRUE), "plotSample_case_1.rds")
})

test_that("sample type is SRS, lattice=TRUE", {
  expect_known_value(plotSample(sdraw(HAL, 25, type="SRS"), WY, lattice=TRUE ), "plotSample_case_2.rds")
})

test_that("sample type is SSS, poly.fill= FALSE", {
  expect_known_value(plotSample(sdraw(WY, 10, type="SSS", spacing=c(1,1)), WY, lattice=TRUE, poly.fill= FALSE), "plotSample_case_3.rds")
})

test_that("x is SpatialLines", {
  expect_known_output(plotSample(sdraw(Sl, 25, type="BAS"),  lattice=TRUE ), "plotSample_case_4.rds")
  expect_warning(plotSample(sdraw(Sl, 25, type="BAS"), WY, lattice=TRUE ), "bbox not plotted. bbox is not 2-dimensional for 1D balanced line samples")
})

test_that("x is SpatialPoints", {
  expect_known_value(plotSample(sdraw(spObj, 25, type="BAS"), lattice=TRUE ), "plotSample_case_5.rds")
})
