hal.polygon <- function( n, shp, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3)){
  #
  #   Draw a HAL sample from a polygon in a shapefile.
  #
  #   input:
  #   n = desired sample size,
  #   shp = a SpatialPolygons* object, according to package sp.
  #   J = 2X1 vector of base powers.  J[1] is for horizontal, J[2] for vertical dimension
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   eta = 2X1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     horizontal dimension, eta[2] is for vertical dimension. 
  #   triangular = boolean, if TRUE, construct a triangular grid. If FALSE, construct rectangluar grid.
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 
  
  #   Output:
  #   A SpatialPOints* object containing n HAL sample points, in order. 

  #   Details
  #   If J is missing, the approximate frame size or approximate lattice size is set to 100 times 
  #     the number of samples requested. If J is NULL, the algorithm attempts to place 100*n points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  The lattice size is not exact, but is a target. 
  
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # If lattice is not specified, set frame size to some multiple of n
  if(is.null(J)){
    N <- 100*n
  } else {
    N <- NULL
  }
  
  # Construct Halton lattice  
  hl.points <- halton.lattice.polygon( shp, N, J, eta, triangular, bases )
  
  # Now that we have points, we can draw a HAL point sample. 
  samp <- hal.point( n, hl.points, attr(hl.points, "J"), attr(hl.points, "bases") )
  
  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
