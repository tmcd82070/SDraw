#' @export halton.lattice
#' 
#' @title Halton lattice inside a rectangle
#' 
#' @description Constructs a lattice of Halton boxes (a Halton lattice) inside 
#' a rectangular box. 
#' 
#' @details This is designed to be called with the bounding box of a spatial 
#' object.  See examples.
#' 
#' \bold{Definition of Halton lattice}: A Halton lattice has the same number 
#' of points in every Halton box.  Halton boxes are the \code{bases[1]^J[1]} X 
#' \code{bases[2]^J[2]} matrix of rectangles over a square. Each Halton box 
#' contains \code{prod(eta)} points. 
#' 
#' @param box A DX2 matrix containing coordinates of the box. 
#' One row per dimension. Column 1 is the minimum, column 2 is the maximum. 
#' \code{box[1,]} contains \code{c(min,max)} coordinates of the box in dimension 1
#' (horizontal).  \code{box[2,]} contains \code{c(min,max)} coordinates of 
#' the box in dimension 2 (vertical). Etc for higher dimensions.  
#' Default is the 2D unit box.
#' 
#' @param J A DX1 vector of base powers which determines the size and shape 
#' of the Halton boxes. Elements of \code{J} less than or equal 
#' to 1 are re-set to 1. See additional description in help for 
#' \code{\link{hip.polygon}} function.  
#' 
#' @param N Approximate number of points to place in the whole box.  If \code{J} 
#' is specified, it takes precedence.  If \code{J} is NULL, the 
#' algorithm attempts to place \code{N} points in the bounding box 
#' using Halton boxes that are as close to square as possible.  
#' \code{N} is not exact, but is a target. 
#' 
#' @param eta A DX1 vector of the number of points to add inside each Halton box.  
#' e.g., if \code{eta} = \code{c(3,2)}, a small grid of 3 by 2 points is 
#' added inside each Halton box. \code{eta[1]} is for the
#' horizontal dimension, \code{eta[2]} is for the vertical dimension, etc for 
#' higher dimensions. 
#' 
#' @param triangular boolean, if TRUE, construct a triangular grid. 
#' If FALSE, construct rectangular grid.  See help for \code{\link{hip.polygon}}.
#' 
#' @param bases A DX1 vector of Halton bases.  These must be co-prime. 
#' 
#' @return A data frame containing coordinates in the Halton lattice. 
#' Names of the coordinates are \code{dimnames(box)[1]}.  If \code{box} does not 
#' have dimnames, names of the coordinates are \code{c("d1", "d2", ...)} (d1 is 
#' horizontal, d2 is vertical, etc).
#' 
#' In addition, return has following attributes:
#' \itemize{
#'    \item \code{J}: the \code{J} vector used to construct the lattice. 
#'      This is either the input \code{J} or the computed \code{J} when 
#'      only \code{N} is specified. 
#'    \item \code{eta}: the \code{eta} vector used in the lattice.
#'    \item \code{bases}: Bases of the van der Corput sequences used in the lattice, 
#'      one per dimension.
#'    \item \code{triangular}: Whether the lattice is triangular or square.
#'    \item \code{hl.bbox}: The input \code{box}.  If \code{box} does not 
#'    have dimnames, this attribute will be assigned dimnames of 
#'    \code{list(c("d1","d2"),c("min","max"))}. 
#' }
#'
#' @author Trent McDonald
#' 
#' @seealso \code{\link{halton.lattice}}, \code{\link{hip.polygon}}
#' 
#' @examples 
#' 
#' # Lattice of 2^3*3^2 = 72 points in unit box
#' hl <- halton.lattice( J=c(3,2) )
#' 
#' # Plot
#' hl.J <- attr(hl,"J")
#' hl.b <- attr(hl,"bases")
#' hl.bb <- attr(hl,"hl.bbox") 
#'
#' plot( hl.bb[1,], hl.bb[2,], type="n", pty="s")
#' points( hl[,1], hl[,2], pch=16, cex=.75, col="red")
#' 
#' for(d in 1:ncol(hl)){
#'   tmp2 <- hl.bb[d,1] + (0:(hl.b[d]^hl.J[d]))*(diff(hl.bb[d,]))/(hl.b[d]^hl.J[d])
#'   if( d == 1){
#'       abline(v=tmp2)
#'   } else{
#'       abline(h=tmp2)
#'   }
#' }
#' 
#' # Lattice of approx 1000 points over bounding box of spatial object
#' hl <- halton.lattice( bbox(HI.coast), N=1000 )

halton.lattice <- function(box=matrix(c(0,0,1,1),2), N=10000, J=NULL, 
                           eta=rep(1,nrow(box)), triangular=FALSE, bases=NULL){

  D <- nrow( box )   # number of dimensions
  
  delta <- apply( box, 1, diff )   # size/extent of box in each dimension
  
  ll.corner <- apply(box, 1, min)  # minimum coordinate of box in each dimension
  
  if(is.null(bases)){
    bases <- primes(D)
  } else if(length(bases)!=D){
    stop("Dimensions must equal length of bases. Make nrow(box) == length(bases)")
  }
  
  if( length(eta) != D) stop("Dimensions must equal length of Eta parameter.")
  
  if( triangular & (D!=2)) warning("Triangular grids for D!=2 not implemented. Rectangular grid produced.")
  
  # it is interesting to set elements of J to non-integers

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
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0
    n.boxes <- bases^J
  }
  

  # Inflate n.boxes by eta (n.boxes is number of halton boxes in each dimension, now we need 
  # number of points)
  n <- eta * n.boxes
  
  # Construct sequences in each direction
  coords <-vector("list",D)
  if( is.null(dimnames(box)[[1]]) ){
    names(coords) <- paste("d",1:D, sep="")
    dimnames(box) <- list(paste("d",1:D, sep=""),c("min","max"))
  } else {
    names(coords) <- dimnames(box)[[1]]
  }
  for( i in 1:D ){
    c.seq <- seq( 1, n[i] )
    coords[[i]] <- (c.seq - 0.5)/n[i]
    coords[[i]] <- ll.corner[i] + coords[[i]]*delta[i]
  }
  
  # Expand the grid
  hl.coords <- expand.grid( coords, KEEP.OUT.ATTRS = FALSE )

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
  attr(hl.coords,"hl.bbox") <- box
  attr(hl.coords,"triangular") <- triangular
  
  hl.coords
}

