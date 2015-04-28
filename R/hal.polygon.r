hal.polygon <- function( n, shp, N=50000, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3)){
  #
  #   Draw a HAL sample from a polygon in a shapefile.
  #
  #   input:
  #   n = desired sample size,
  #   shp = a SpatialPolygons* object, according to package sp.
  #   J = 2X1 vector of base powers.  J[1] is for horizontal, J[2] for vertical dimension
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   N = Approximate number of points to place in the lattice.  If J is specified, it 
  #     takes precedence.  If J is NULL, the algorithm attempts to place N points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  This N is not exact, but is a target. 
  #   eta = 2X1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     horizontal dimension, eta[2] is for vertical dimension. 
  #   triangular = boolean, if TRUE, construct a triangular grid. If FALSE, construct rectangluar grid.
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 
  
  #   Output:
  #   A SpatialPOints* object containing n HAL sample points, in order. 
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # Construct Halton lattice
  hl.points <- halton.lattice.polygon( sp.obj, N, J, eta, triangular, bases )
  
  # Assign Halton indicies to points.  This returns a SpatialPoints* object
  hl.points <- halton.indicies( hl.points )
  
  # Make a Halton frame, which takes halton.index and adds cycles to points in same Halton box
  # This frame comes back sorted by halton order, ready to sample
  hl.points <- halton.frame( hl.points )
  
  # Draw sample from the frame
  m <- runif(200)    # burn 200 random numbers from R's routine
  N.frame <- nrow(hl.points)
  m <- floor(runif(1, 0, N.frame))
  n <- min( n, N.frame )  # Can't take more than a census. 
  ind <- ((((1:n)+m)-1) %%  N.frame)+1   # Cycle the indicies around to start of frame if necessary

  samp <- hl.points[ind,]
  
  samp
  
  
}
