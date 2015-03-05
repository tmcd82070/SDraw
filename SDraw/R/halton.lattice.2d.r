halton.lattice.2d <- function(bbox=matrix(c(0,0,1,1),2), N=10000, J=NULL, eta=rep(1,nrow(bbox)), triangular=FALSE, bases=NULL){
  # 
  # Return coordinates in a 2d Halton lattice, as a set of (x,y) vectors 
  #
  # Input: 
  #   bbox = Dx2 matrix equal to the bounding box for the Halton lattice. bbox[1,] = c(min, max) of dimension 1, bbox[2,] = c(min, max)
  #     of dimension 2, etc. Default is the unit box [0,1]X[0,1].  Number rows is number of dimensions. 
  #   J = 2X1 vector of base powers.  J[1] is for dimention 1, J[2] for dimension 2, etc.
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   N = Approximate number of points to place in the lattice (dim(1)*dim(2)).  If J is specified, it 
  #     takes precedence.  If J is NULL, the algorithm attempts to place N points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  This N is not exact, but is a target. 
  #   eta = 2X1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     dimension 1, eta[2] is for dimension 2, etc. 
  #   triangular = boolean, if TRUE, construct a triangular grid. If FALSE, construct rectangluar grid.
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 

  D <- nrow( bbox )   # number of dimensions
  
  delta <- apply( bbox, 1, diff )   # size/extent of box in each dimension
  
  if(is.null(bases)){
    bases <- primes(D)
  } else if(length(bases)!=D){
    stop("Number of dimensions must equal length of bases. Make nrow(bbox) == length(bases)")
  }
  
  if(is.null(J)){
    # Set default values of J so Halton boxes are as close to squares as possible
    n <- rep(NA,D)
    for( i in 1:D ){
        n[i] <- ((delta[i]^(D-1))/prod(delta[-i]) * N)^(1/D)
    }

    print(n)
    
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
  
  # Inflate n by eta (before this, n was number of halton boxes in each dimension, now it 
  # will be number of points)
  n <- eta * n
  
  # Construct sequences in each direction
  coords <- matrix(NA, prod(n), D )
  for( i in 1:2 ){
    c.seq <- seq( 1, n[i] )
    coords <- (c.seq - 0.5)/n[i]
  }
  x.coords <- 
  
  print(delta / n)
  cat("------\n")
  print(J)
  print( bases^J )
  print( prod(bases^J) )
  n
}

tmp <- halton.lattice.2d(bbox(WA))