##Create spatial points object
##Load pre-built dataset
data(meuse)
data(WY)

##Prepare the coordinates
coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)

##Create some arbitrary lines
l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))

##Creat some formal class lines with these lines
Sl1 <- Line(l1)
Sl2 <- Line(l2)

##Assign arbitrary ID's to these formal class lines
S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")

##Create a formal class SpatialLines object
Sl <- SpatialLines(list(S1,S2))

##Create spatial polygons object
##Make "squares" with integer values rounded in a list
square <- rbind( c(2, 4, 3, 4, 3, 5,
                   2, 5, 2, 4, 2, 4),
                 c(6, 9, 7, 9, 7, 8,
                   6, 8, 6, 9, 6, 9))
##Give these "squares" an identification
ID <- c("shape1", "shape2")

##Create SpatialPolygon object from these squares 
spatPoly <- SpatialPolygons(list(
  Polygons(list(Polygon(matrix(square[1, ], ncol = 2, byrow = TRUE))), ID[1]),
  Polygons(list(Polygon(matrix(square[2, ], ncol = 2, byrow = TRUE))), ID[2])
))

HAL <- halton.lattice.polygon(WY, J=c(3,2), eta=c(3,2), triangular=TRUE )


context("Testing plotSample()")

test_that("sample type is BAS", {
  tmp <- tempfile()
  expect_known_output(plotSample(sdraw(WY, 50, type="BAS"), WY ), tmp, print = TRUE)
})

test_that("sample type is BAS", {
  tmp <- tempfile()
  expect_known_output(plotSample(sdraw(spatPoly, 50, type="BAS"), WY ), tmp, print = TRUE)
})

test_that("sample type is SRS, lattice=TRUE", {
  tmp <- tempfile()
  expect_known_output(plotSample(sdraw(HAL, 25, type="SRS"), WY, lattice=TRUE ), tmp, print = TRUE)
})

test_that("sample type is SSS, poly.fill= FALSE", {
  tmp <- tempfile()
  expect_known_output(plotSample(sdraw(WY, 10, type="SSS", spacing=c(1,1)), WY, lattice=TRUE, poly.fill= FALSE), tmp, print = TRUE)
})

test_that("x is SpatialLines", {
  tmp <- tempfile()
  expect_known_output(plotSample(sdraw(Sl, 25, type="BAS"),  lattice=TRUE ), tmp, print = TRUE)
  expect_warning(plotSample(sdraw(Sl, 25, type="BAS"), WY, lattice=TRUE ), "bbox not plotted. bbox is not 2-dimensional for 1D balanced line samples")
})

test_that("x is SpatialPoints", {
  tmp <- tempfile()
  expect_known_output(plotSample(sdraw(spObj, 25, type="BAS"),  lattice=TRUE ), tmp, print = TRUE)
})
