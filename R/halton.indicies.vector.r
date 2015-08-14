#' @export halton.indicies.vector
#'  
#' @title Halton indicies for an entire vector of coordinates 
#' 
#' @description Computes Halton indicies of an entire vector of points by matching 
#' them with a vector of the Halton sequence. 
#' This function is relatively fast, but can only handle reasonably sized vectors. 
#' 
#' @param hl.coords nXD vector of coordinates for points 
#' @param n.boxes DX1 vector containing number of Halton boxes in each dimension.
#' @param D Number of dimensions
#' @param b DX1 vector of bases to use for each dimension
#' @param delta DX1 vector of study area extents in each dimension.  Study area is 
#'  \code{delta[i]} units wide in dimension \code{i}.
#' @param ll.corner DX1 vector containing minimum coordinates in all dimensions.
#' 
#' @return A nX1 vector of Halton indicies corresponding to points in \code{hl.coords}. 
#' 
#' @details The Halton sequence maps the non-negative integers to D-space.  
#' Halton indicies are invert of the Halton sequence. The Halton index of a point 
#' is the index of the Halton box that contains the point.  The Halton index of a point
#' is an integer N such that integers in the set $\{x:N = x mod C\}$, where $C$ 
#' = \code{prod(n.boxes)}, 
#' get mapped by the Halton sequence to  the same box
#' Halton box as the original point.  
#' 
#' This routine uses the Halton sequence and modular arithmetic to find Halton indicies.
#' This means several vectors of size \code{nrow(hl.coords)} must be created.  Depending on 
#' memory, this approach fails for a sufficently large number of points. When this routine 
#' fails, see the slower \code{\link{F.halton.indicies.Chinese}}, which computes indicies by solving 
#' the Chinese Remainder Theorem. 
#' 
#' @author Trent McDonald
#' 
#' 
#' @seealso \code{\link{F.halton.indicies.Chinese}}, \code{\link{F.halton.indicies}}
#' 
#' @examples 
#' pt <- data.frame(x=0.43, y=0.64)
#' n.boxes <- c(16,9) 
#' halton.indicies.vector(pt, n.boxes) # should equal 70
#' 
#' pt <- data.frame(x=143, y=164)
#' halton.indicies.vector(pt, n.boxes, delta=c(100,100), ll.corner=c(100,100)) # should also equal 70
#' 
#' # Plot Halton boxes and indicies to check
#' b <- c(2,3)
#' J <- c(4,2)
#' hl.ind <- halton( prod(n.boxes), 2,0 )
#' plot(c(0,1),c(0,1),type="n")
#' for( i in J[1]:1) abline(v=(0:b[1]^i)/b[1]^i, lwd=J[1]+1-i, col=i)
#' for( i in J[2]:1) abline(h=(0:b[2]^i)/b[2]^i, lwd=J[2]+1-i, col=i)
#' for( i in 1:prod(n.boxes)){
#'   box.center <- (floor(n.boxes*hl.ind[i,]+.Machine$double.eps*10) + 1-.5)/n.boxes
#'   text(box.center[1],box.center[2], i-1, adj=.5)  
#' }
#' points(pt$x, pt$y, col=6, pch=16, cex=2)
#' 
#' 
halton.indicies.vector <- function(hl.coords, n.boxes, D=2, b=c(2,3), delta=c(1,1), ll.corner=c(0,0)){

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
  hl.ind <- hl.vec[cell.coord]

  hl.ind

}