halton.lattice.2d <- function(bbox=matrix(c(0,0,1,1),2), J=NULL, N=10000, eta=c(1,1), bases=c(2,3)){
  # 
  # Return coordinates in a 2d Halton lattice, as a set of (x,y) vectors 
  #
  # Input: 
  #   bbox = bounding box for the Halton lattice. bbox[1,] = c(min, max) of dimension 1, bbox[2,] = c(min, max)
  #     of dimension 2, etc. Default is the unit box [0,1]X[0,1]
  #   J = 2X1 vector of base powers.  J[1] is for dimention 1, J[2] for dimension 2, etc.
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   N = Approximate number of points to place in the lattice (dim(1)*dim(2)).  If J is specified, it 
  #     takes precedence.  If J is NULL, the algorithm attempts to place N points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  This N is not exact, but is a target. 
  #   eta = 2X1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     dimension 1, eta[2] is for dimension 2, etc. 
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 

  rng.x <- bbox[1,]
  rng.y <- bbox[2,]
  
  dx <- diff(rng.x)
  dy <- diff(rng.y)
  
  if(is.null(J)){
    # Set default values of J so Halton boxes are as close to squares as possible
    n.x <- sqrt(N * dx/dy)
    n.y <- sqrt(N * dy/dx)
    # round the n closest to an integer
    n <- c(n.x=n.x, n.y=n.y)
    
    # compute J which gives something close to n
    J <- round( log(n)/log(bases) )
    n <- bases^J
    
    #print(round(n))
    #tmp <-  n - floor(n)
    #tmp2 <- which.min(apply(cbind(tmp, 1-tmp ), 1, min))
    #n[tmp2] <- round(n[tmp2])
    #n[-tmp2] <- round( sqrt(N * c(dx,dy)[-tmp2]/c(dx,dy)[tmp2]) )
  } else {
    n <- bases^J
  }
  

  print(prod(n))
  print(n)
  cat("------\n")
  print(J)
  print( bases^J )
  print( prod(bases^J) )
  n
}

tmp <- halton.lattice.2d(bbox(WA))