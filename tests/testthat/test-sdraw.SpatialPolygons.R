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


context("Test the sdraw.SpatialPolygons function")

test_that("sdraw.SpatialPolygons() operates appropriately", {
  
  # check if the function stops with message
  expect_error(sdraw.SpatialPolygons(spatPoly, 15, type="HOP"),"Invalid SpatialPolygons sample type = HOP",fixed=TRUE )
  expect_error(sdraw.SpatialPolygons(spatPoly, 20, type="BTS"),"Invalid SpatialPolygons sample type = BTS",fixed=TRUE )
  expect_error(sdraw.SpatialPolygons(spatPoly, 25, type="SAS"),"Invalid SpatialPolygons sample type = SAS",fixed=TRUE )
  expect_error(sdraw.SpatialPolygons(spatPoly, 30, type="STS"),"Invalid SpatialPolygons sample type = STS",fixed=TRUE )
  expect_error(sdraw.SpatialPolygons(spatPoly, 35, type="RTS"),"Invalid SpatialPolygons sample type = RTS",fixed=TRUE )
  
  # check the output and length
  expect_length(sdraw.SpatialPolygons(spatPoly,10, type ="GRTS"),length(primes(10)))
  
})


