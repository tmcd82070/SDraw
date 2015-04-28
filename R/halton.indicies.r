halton.indicies <- function(hl){
  #
  # Compute and attach indicies of the Halton sequence to points in the 
  # input Halton lattice. 
  #
  # Input: 
  #   hl = either a data frame or a SpatialPoints* object. Suitable input is the output of 
  #     functions halton.lattice() (a data frame) and halton.lattice.polygon() (a SpatialPoints* object).  
  #     If hl is a data frame, it must contain coordinates columns named the same as dimnames[[1]] of the 
  #     bounding box attribute.  That is, coordinates are assumed to be hl[dimnames(bb)[[1]]], where bb
  #     is the bounding box attribute of hl (see attributes below).  
  #
  #   The data frame and SpatialPOints* object must have the following attributes:   
  #     "J" = levels or exponents of bases used in halton lattice; 
  #     "eta" = number of points in x and y direction in each halton box; 
  #     "bases" = bases in each dimension used in halton lattice; 
  #     "hl.bbox" = bounding box of the full lattice. 
  #
  # Output: 
  #   a data frame line hl, but with indicies of the Halton sequence attached in column "hl.index". 
  #
 
  J <- attr(hl,"J")
  b <- attr(hl,"bases")
  bb <- attr(hl,"hl.bbox") 
  eta <- attr(hl,"eta")
  
  
  # Number of dimensions
  D <- nrow(bb)
  
  # size/extent of box in each dimension
  delta <- apply( bb, 1, diff )   
  
  # minimum coordinate of bbox in each dimension
  ll.corner <- apply(bb, 1, min)  
  
  
  # This is number of halton boxes in each direction
  n.boxes <- b ^ J
  
  # Lattice coordinates
  if( regexpr("SpatialPoints", class(hl)) > 0 ){
    hl.coords <- coordinates(hl)
  } else {
    hl.coords <- hl[,dimnames(bb)[[1]]]
  }
  
  # Halton index matrix, stored as a vector in column-major order for now.  
  # Each cell cooresponds to a Halton box. 
  hl.vec <- rep(NA, prod(n.boxes))
  
  # Fill Halton matrix with Halton indicies
  hl.ind <- halton( prod(n.boxes), D)
  
  # This sets up the column major ordering of our array. 
  # Do it this way because it generalized to D > 2
  mat.sz <- cumprod( c(1,n.boxes[-length(n.boxes)]) )
  m1 <- c(0, rep(1,D-1))   # vector with 0 in first element, 1's after

  hl.ind <- t(hl.ind)
  cell.coord <- floor(hl.ind * n.boxes + .Machine$double.eps*10) + 1
  cell.coord <- colSums( (cell.coord - m1)*mat.sz )
  hl.vec[cell.coord] <- 1:ncol(hl.ind)
  

  # Find the Halton boxes that the points in hl belong in
  hl.coords <- t(hl.coords)
  cell.coord <- floor( n.boxes*(hl.coords - ll.corner)/(delta) + .Machine$double.eps*10) + 1
  cell.coord <- colSums( (cell.coord - m1)*mat.sz )
  hl.out <- data.frame( data.frame(hl), halton.index=hl.vec[cell.coord] )

  if( regexpr("SpatialPoints", class(hl)) > 0 ){
    # Return a SpatialPoints* object
    hl.out <- SpatialPointsDataFrame(coordinates(hl), data=hl.out)
  } 
  
  hl.out
  
}

