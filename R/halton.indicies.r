halton.indicies <- function(hl, index.name="halton.index"){
  #
  # Compute and attach indicies of the Halton sequence to points in the 
  # input Halton lattice. 
  #
  # Input: 
  #   hl = either a data frame or a SpatialPoints* object. Suitable input is the output of 
  #     functions halton.lattice() (a data frame) and halton.lattice.polygon() (a SpatialPoints* object).  
  #     If hl is a data frame, it must contain coordinates columns named the same as dimnames[[1]] of the 
  #     bounding box attribute.  That is, coordinates are assumed to be hl[dimnames(bb)[[1]]], where bb
  #     is the bounding box attribute of hl (see attributes below). If hl is a data.frame, this function 
  #     works for D>2. If hl is a SpatialPoints* object, it only works for D=2 because SpatialPoints are
  #     only defined in 2D.
  #
  #   The data frame and SpatialPOints* object must have the following attributes:   
  #     "J" = levels or exponents of bases used in halton lattice; 
  #     "eta" = number of points in x and y direction in each halton box; 
  #     "bases" = bases in each dimension used in halton lattice; 
  #     "hl.bbox" = bounding box of the full lattice. 
  #
  #   index.name = name of the column in the output data frame or SpatialPoints* object containing
  #     the Halton indicies.  This is saved as an attribute of the output object
  #
  # Output: 
  #   a data frame like hl, but with indicies of the Halton sequence attached in column "hl.index". 
  #
 
  J <- attr(hl,"J")
  b <- attr(hl,"bases")
  bb <- attr(hl,"hl.bbox") 
  eta <- attr(hl,"eta") # not used here, but saved to transfer to attributes of output object
  triangle <- attr(hl,"triangular")   # not used here, but saved to transfer to attributes of output object
  
  
  # Number of dimensions
  D <- nrow(bb)
  
  # size/extent of box in each dimension
  delta <- apply( bb, 1, diff )   
  
  # minimum coordinate of bbox in each dimension
  ll.corner <- apply(bb, 1, min)  
  
  
  # This is number of halton boxes in each direction
  n.boxes <- b ^ J
  
  # Lattice coordinates.  After this, hl is a data.frame or NULL
  if( regexpr("SpatialPoints", class(hl)) > 0 ){
    hl.coords <- coordinates(hl)
    p4string <- CRS(proj4string(hl))  # will need this later when convert back to SpatialPoints
    if( regexpr("SpatialPointsDataFrame", class(hl)) > 0 ){
      hl <- hl@data  # extract from slot because we don't want extra copy of coordinates
    } else {
      hl <- NULL
    }
    is.points <- TRUE
  } else {
    hl.coords <- hl[,dimnames(bb)[[1]]]
    is.points <- FALSE
  }
  
  # Note, the following is a nice way around the Chinese remainder theorem. 
  # In general, if using another sequence, we would be solving the Chinese remainder
  # theorem here. 
  
  # Halton index matrix, stored as a vector in column-major order for now.  
  # Each cell cooresponds to a Halton box. 
  hl.vec <- rep(NA, prod(n.boxes))
  
  # Fill Halton matrix with Halton indicies
  hl.ind <- halton( prod(n.boxes), D, start=0, bases=b)
  
  # This sets up the column major ordering of our array. 
  # Do it this way because it generalizes to D > 2
  mat.sz <- cumprod( c(1,n.boxes[-length(n.boxes)]) )
  m1 <- c(0, rep(1,D-1))   # vector with 0 in first element, 1's after

  hl.ind <- t(hl.ind)
  cell.coord <- floor(hl.ind * n.boxes + .Machine$double.eps*1000) + 1
  cell.coord <- colSums( (cell.coord - m1)*mat.sz )
  hl.vec[cell.coord] <- 0:(ncol(hl.ind)-1)
  

  # Find the Halton boxes that the points in hl belong in
  hl.coords <- t(hl.coords)
  cell.coord <- floor( n.boxes*(hl.coords - ll.corner)/(delta) + .Machine$double.eps*1000) + 1
  cell.coord <- colSums( (cell.coord - m1)*mat.sz )
  if(is.null(hl)){
    hl.out <- data.frame(halton.index=hl.vec[cell.coord])
    names(hl.out) <- index.name
  } else {
    hl.out <- data.frame( hl, halton.index=hl.vec[cell.coord] )
    names(hl.out) <- c(names(hl), index.name)
  }

  if( is.points ){
    # Return a SpatialPoints* object
    hl.out <- SpatialPointsDataFrame(t(hl.coords), data=hl.out, proj4string=p4string )
  } 
  
  # Add attributes
  attr(hl.out,"J") <- J
  attr(hl.out,"eta") <- eta
  attr(hl.out,"bases") <- b
  attr(hl.out,"hl.bbox") <- bb
  attr(hl.out,"triangular") <- triangle
  attr(hl.out,"index.name") <- index.name
  
  
  hl.out
  
}

