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
  

  # Find the Halton boxes that the points in hl below in
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


# -------------- Some Example code -------------------------
# tmp <- halton.lattice(bbox(WA.utm), N=220, J=c(4,2), eta=c(2,2), triangular=T)
# tmp2 <- halton.indicies( tmp )
# 
# plot(tmp)
# for( i in 1:max(tmp2$halton.index)){
#   points(tmp2[tmp2$halton.index==i,], pch=16,col=rainbow(max(tmp2$halton.index))[i])
# }

tmp <- halton.lattice.polygon(WA.utm[3,], J=c(6,3), eta=c(2,2))
tmp2 <- halton.indicies(tmp)

plot(WA.utm, xlim=c(480118.3, 515610.1), ylim=c(5230959 ,5265726))
plot(WA.utm)
for( i in 1:max(tmp2$halton.index)){
  if( sum( tmp2$halton.index==i)>0){
    points(tmp2[tmp2$halton.index==i,c("x","y")], pch=16,col=rainbow(max(tmp2$halton.index))[i])
  }
}




# -------------- Some exploratory code ---------------------



##  This plots the halton boxes, points, and indices for particular J's
J <- c(4,2)
b <- c(2,3)
n.boxes <- b ^ J
hl.ind <- halton( prod(n.boxes), 2 )

plot(c(0,1),c(0,1),type="n")

for( i in J[1]:1){
  abline(v=(0:b[1]^i)/b[1]^i, lwd=J[1]+1-i, col=i)
}
for( i in J[2]:1){
  abline(h=(0:b[2]^i)/b[2]^i, lwd=J[2]+1-i, col=i)
}
points(hl.ind[,1], hl.ind[,2], col=6, pch=16)


for( i in 1:prod(n.boxes)){
  tmp2 <- (floor(n.boxes*hl.ind[i,]+.Machine$double.eps*10) + 1-.5)/n.boxes
  text(tmp2[1],tmp2[2], i, adj=.5, col="black")  
}
