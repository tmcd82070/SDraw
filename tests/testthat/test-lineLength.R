##Create spatial points object
##Load pre-built dataset
data(meuse)

##Prepare the coordinates
coords <- meuse[ , c("x", "y")]

##Assign
spObj <- SpatialPoints(coords)

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


context("Testing lineLength()")

test_that("x is a SpatialPoints", {
  expect_equal(lineLength(spObj), 0)
})

test_that("lineLength(spatPoly) is 8",{
  expect_equal(lineLength(spatPoly), length(primes(8)))
})

test_that("lineLength(Sl) is 4.650282",{
  expect_equal(round(lineLength(Sl),6), 4.650282)
})