hal.polygon <- function( n, sp.obj, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3)){
  #
  #   Draw a HAL sample from a polygon in a shapefile.
  #
  #   input:
  #   n = desired sample size,
  #   sp.obj = a SpatialPolygons* object, according to package sp.
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

  print(N.frame)
  
  samp <- hl.points[ind,]
  
  # Add attributes
  attr(samp,"J") <- attr(hl.points, "J")
  attr(samp,"eta") <- attr(hl.points, "eta")
  attr(samp,"bases") <- attr(hl.points, "bases")
  attr(samp,"hl.bbox") <- attr(hl.points, "hl.bbox")
  attr(samp,"triangular") <- attr(hl.points, "trianglular")
  attr(samp,"index.name") <- attr(hl.points,"index.name")
  attr(samp,"order.name") <- attr(hl.points,"order.name")
  attr(samp,"m") <- m  # needed if we want more points from this frame
  
  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
