halton.indicies <- function(hl){
  #
  # Compute and attach indicies of the Halton sequence to points in the 
  # input Halton lattice. 
  #
  # Input: 
  #   hl = either a data frame or a SpatialPoints* object. Suitable input is the output of 
  #     functions halton.lattice() (a data frame) and halton.lattice.polygon() (a SpatialPoints* object).  
  #     If hl is a data frame, it must contain at coordinates columns named the same as dimnames[[1]] of the 
  #     bounding box attribute.  That is, coordinates are assumed to be hl[dimnames(bb)[[1]]], where bb
  #     is the bounding box attribute of hl.  The data frame and SpatialPOints* object must have the 
  #     following attributes:   "J" = levels or exponents of bases used in halton lattice; 
  #     "eta" = number of points in x and y direction in each halton box; "bases" = bases
  #     in each dimension used in halton lattice; "hl.bbox" = bounding box of the full lattice. 
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
  
  # This is number of halton boxes in each direction
  n.boxes <- b ^ J
  
  # Lattice coordinates
  if( regexpr("SpatialPoints", class(hl)) > 0 ){
    hl.coords <- coordinates(hl)
  } else {
    hl.coords <- hl[,dimnames(bb)[[1]]]
  }
  
  # Halton matrix.  Each cell cooresponds to a Halton box. 
  hl.mat <- array(NA, n.boxes)
  
  # Fill Halton matrix with Halton indicies
  hl.ind <- halton( prod(n.boxes), D)
  
  for( i in 1:nrow(hl.ind) ){
    cell.coord <- floor(hl.ind[i,] * n.boxes)
    hl.mat[cell.coord] <- i
  }
  
}