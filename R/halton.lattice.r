halton.lattice <- function(bbox=matrix(c(0,0,1,1),2), N=10000, J=NULL, eta=rep(1,nrow(bbox)), triangular=FALSE, bases=NULL){
  # 
  # Return coordinates in a D-dimensional Halton lattice, as a set of vectors in a data frame.
  #
  # Input: 
  #   bbox = Dx2 matrix equal to the bounding box for the Halton lattice. bbox[1,] = c(min, max) of dimension 1, bbox[2,] = c(min, max)
  #     of dimension 2, etc. Default is the unit box [0,1]X[0,1].  Number rows is number of dimensions.  dimnames(bbox)[[1]] is the name
  #     of the columns on output (i.e., coordinate names). 
  #   J = DX1 vector of base powers.  J[1] is for dimention 1, J[2] for dimension 2, etc.
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   N = Approximate number of points to place in the lattice total.  If J is specified, it 
  #     takes precedence.  If J is NULL, the algorithm attempts to place N points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  This N is not exact, but is a target. 
  #   eta = DX1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     dimension 1, eta[2] is for dimension 2, etc. 
  #   triangular = boolean, if TRUE, construct a triangular grid. If FALSE, construct rectangluar grid.
  #   bases = DX1 vector of Halton bases.  These must be co-prime. If bases = NULL (the default), the 
  #     first D prime integers are used. 
  # 
  # Output: 
  # a data frame of halton lattice points. 

  D <- nrow( bbox )   # number of dimensions
  
  delta <- apply( bbox, 1, diff )   # size/extent of box in each dimension
  
  ll.corner <- apply(bbox, 1, min)  # minimum coordinate of bbox in each dimension
  
  if(is.null(bases)){
    bases <- primes(D)
  } else if(length(bases)!=D){
    stop("Dimensions must equal length of bases. Make nrow(bbox) == length(bases)")
  }
  
  if( length(eta) != D) stop("Dimensions must equal length of Eta parameter.")
  
  if( triangular & (D!=2)) warning("Triangular grids for D!=2 not implemented. Rectangular grid produced.")
  
  
  if(is.null(J)){
    # Compute n.boxes, because prod(eta) points are added inside each halton box and we desire N points
    N.boxes <- N / prod(eta)

    # Set default values of J so Halton boxes are as close to squares as possible
    n.boxes <- rep(NA,D)  # n boxes in each dimension
    for( i in 1:D ){
        n.boxes[i] <- ((delta[i]^(D-1))/prod(delta[-i]) * N.boxes)^(1/D)
    }

    
    # compute J which gives something close to n
    J <- round( log(n.boxes)/log(bases) )
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0
    n.boxes <- bases^J
    
    #print(round(n))
    #tmp <-  n - floor(n)
    #tmp2 <- which.min(apply(cbind(tmp, 1-tmp ), 1, min))
    #n[tmp2] <- round(n[tmp2])
    #n[-tmp2] <- round( sqrt(N * c(dx,dy)[-tmp2]/c(dx,dy)[tmp2]) )
  } else {
    n.boxes <- bases^J
  }
  
  # Inflate n.boxes by eta (n.boxes is number of halton boxes in each dimension, now we need 
  # number of points)
  n <- eta * n.boxes
  
  # Construct sequences in each direction
  coords <-vector("list",D)
  if( is.null(dimnames(bbox)[[1]]) ){
    names(coords) <- paste("d",1:D, sep="")
    dimnames(bbox) <- list(paste("d",1:D, sep=""),c("min","max"))
  } else {
    names(coords) <- dimnames(bbox)[[1]]
  }
  for( i in 1:D ){
    c.seq <- seq( 1, n[i] )
    coords[[i]] <- (c.seq - 0.5)/n[i]
    coords[[i]] <- ll.corner[i] + coords[[i]]*delta[i]
  }
  
  # Expand the grid
  hl.coords <- expand.grid( coords )

  # Make triangular if called for
  if(triangular & (D==2)){
    x.tweak <- rep(c(0.25,-0.25)*delta[1]/n[1], each=n[1])  # 2 rows
    x.tweak <- rep( x.tweak, ceiling(n[2]/2) ) # correct length if n[2] even, too long by 1 row if n[2] odd
    x.tweak <- x.tweak[1:prod(n)]
    hl.coords[,1] <- hl.coords[,1] + x.tweak 
  }
  
  # Add attributes
  attr(hl.coords,"J") <- J
  attr(hl.coords,"eta") <- eta
  attr(hl.coords,"bases") <- bases
  attr(hl.coords,"hl.bbox") <- bbox
  attr(hl.coords,"triangular") <- triangular
  
  hl.coords
}

# tmp <- halton.lattice(bbox(WA.utm), N=220, J=c(4,2), eta=c(2,2), triangular=T)
# tmp <- halton.lattice(bbox(WA.utm), N=220, triangular=T)
# 
# tmp.J <- attr(tmp,"J")
# tmp.b <- attr(tmp,"bases")
# tmp.bb <- attr(tmp,"hl.bbox") 
# 
# 
# plot( tmp.bb[1,], tmp.bb[2,], type="n")
# points( tmp[,1], tmp[,2], pch=16, cex=.75, col="red")
# 
# 
# for(d in 1:ncol(tmp)){
#   tmp2 <- tmp.bb[d,1] + (0:(tmp.b[d]^tmp.J[d]))*(diff(tmp.bb[d,]))/(tmp.b[d]^tmp.J[d])
#   if( d == 1){
#       abline(v=tmp2)
#   } else{
#       abline(h=tmp2)
#   }
# }
