hal.point <- function( n, shp, J=NULL, bases=c(2,3)){
  #
  #   Draw a HAL sample from points in a shapefile.
  #
  #   input:
  #   n = desired sample size,
  #   shp = a SpatialPoints* object, according to package sp.
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

    bb <- bbox( shp )
    
    D <- nrow( bb )   # number of dimensions
    
    delta <- apply( bb, 1, diff ) 
    
    # Set default values of J so Halton boxes are as close to squares as possible
    n.boxes <- rep(NA,D)  # n boxes in each dimension
    for( i in 1:D ){
      n.boxes[i] <- ((delta[i]^(D-1))/prod(delta[-i]) * N)^(1/D)
    }
    
    # compute J which gives something close to n
    J <- round( log(n.boxes)/log(bases) )
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0
  }
  
  attr(shp,"J") <- J
  attr(shp,"bases") <- bases
  if( is.null( attr(shp, "hl.box" )) ){
    bb <- bbox(shp)
    # Because Halton boxes are closed on bottom and left, open on top and right, we 
    # need to add a tiny amount to top and right just in case any points are exactly 
    # on the bounding box top or right boundaries. If we did not do this, a point exactly 
    # on the top or right boundary would technically be outside all Halton boxes. 
    delta <- apply( bb, 1, diff ) 
    bb[,2] <- bb[,2] + 0.0001*delta
    attr(shp,"hl.bbox") <- bb
  }
  
  # Compute halton indicies of every point in shp.  The Halton index is the index of the 
  # Halton box that the point falls in. 
  hl.points <- halton.indicies(shp)
  
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
