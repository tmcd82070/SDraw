#' @export halton.indicies.CRT
#'  
#' @title Halton indicies by the Chinese Remainder Theorem (CRT)
#' 
#' @description Computes Halton indicies of D-dimensional points by solving the Chinese Remainder Theorem. 
#' This function is slightly slower than \code{halton.indicies.vector}, but 
#' it works for large problems. 
#' 
#' @param hl.coords nXD vector of coordinates for points. No points can be outside 
#'  the bounding box or exactly on the right or top boundary.  See Details. 
#' @param n.boxes DX1 vector containing number of Halton boxes in each dimension.
#' @param b DX1 vector of bases to use in the Halton sequence.
#' @param delta DX1 vector of study area bounding box extents in each dimension.  Study area is 
#'  bounded by a cube in D space, which is \code{delta[i]} units wide in dimension \code{i}.  Area 
#'  of bounding cube is \code{prod{delta}} units to the \code{D} power. 
#' @param ll.corner DX1 vector containing minimum coordinates in all dimensions.
#' 
#' @return A nX1 vector of Halton indicies corresponding to points in \code{hl.coords}. 
#' 
#' @details The Halton sequence maps the non-negative integers (the Halton indicies) to D-space.  
#' This routine does the inverse.
#' Given a point in D-space and a grid of Halton boxes, the point's Halton index  
#' is any integer N which gets mapped to the Halton box containing the point.  
#' (i.e., any integer in the set $\{x:N = x mod C\}$, where $C$ 
#' = \code{prod(n.boxes)}).  
#' 
#' This routine solves the Chinese Remainder Theorem to find Halton indicies.
#' This routine loops over the points in  \code{hl.coords}, and as such minimizes memory usage 
#' but sacrifices speed. For small problems,  see  \code{\link{halton.indicies.vector}}, 
#' which computes indicies by actually placing points in Halton boxes to find their indicies. 
#' 
#' No point can be less than it's corresponding \code{ll.corner}.  No point 
#' can be equal to or greater than it's corresponding \code{ll.corner + delta}. 
#' 
#' Note: \code{n.boxes} is checked for compatibility with \code{b}.  That is, 
#' \code{log(n.boxes, b)} must all be integers.  
#' 
#' @author Trent McDonald
#' 
#' 
#' @seealso \code{\link{halton.indicies.vector}}, \code{\link{halton.indicies}}
#' 
#' @examples 
#' pt <- data.frame(x=0.43, y=0.64)
#' n.boxes <- c(16,9) 
#' halton.indicies.vector(pt, n.boxes) # should equal 70
#' 
#' # Plot Halton boxes and indicies to check.  
#' # pt should plot in box labeled 70
#' b <- c(2,3)
#' J <- log(n.boxes,b)  # J must be integers
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
#' # Longer vector
#' tmp <- data.frame(x=(0:100)/101,y=.2)
#' n.boxes <- c(16,9)
#' tmp.crt <- halton.indicies.CRT(tmp, n.boxes)
#' 
halton.indicies.CRT <- function(hl.coords, n.boxes, b=c(2,3), delta=c(1,1), 
                                ll.corner=c(0,0)){
  

  # Scale points to [0,1]
  tmp <- ll.corner
  hl.coords <- t( (t(as.matrix(hl.coords)) - ll.corner)/delta )
  
  # Compute J = exponents as a check.
  J <- log(n.boxes, b)
  if( !all( (J - trunc(J)) <= .Machine$double.eps * 1000) ){
    stop( "number of boxes in one or more dimensions is not an integer power of bases. Check n.boxes and b.")
  }
  
  # compute Halton index lookup tables.  Just need 1st cycle here.
  # tmp here should be integers, but round just to remove any fuzz
  tmp <- round( halton(n.boxes[1], bases=b[1])*n.boxes[1] )
  x.hal.order <- data.frame(ord=0:(n.boxes[1]-1), 
                            row=tmp)
  tmp <- round( halton(n.boxes[2], bases=b[2])*n.boxes[2] )
  y.hal.order <- data.frame(ord=0:(n.boxes[2]-1), 
                            row=tmp) 
  
#   print(x.hal.order)
#   print(y.hal.order)
 

  # LL corner of each point's box
  x.n <- floor(hl.coords[,1]*n.boxes[1])
  y.n <- floor(hl.coords[,2]*n.boxes[2])
  
  
  
#   cat("Row - column: ")
#   print(cbind(x.n, y.n))
#   cat("\n")

  # Halton sequence index in each dimension of point. 
  Ax <- sapply(x.n, function(x,hal.ord){ hal.ord$ord[ hal.ord$row == x]}, hal.ord=x.hal.order)
  Ay <- sapply(y.n, function(x,hal.ord){ hal.ord$ord[ hal.ord$row == x]}, hal.ord=y.hal.order)

   # print(cbind(Ax, Ay))
  
  # Solve CRT.  Find N s.t., N = Ax mod n.boxes[1] and N= Ay mod n.boxes[2]
  # This involves taking a modular inverse, which requires the Extended 
  # Euclidean Algorithm to find the greatest common demoninator.
  # Note, because bases are co-prime, gcd should be 1.  We need the s and t 
  # coefficients here.
  gcd <- extended.gcd(n.boxes[1], n.boxes[2])
  
   # print(gcd)
  
  # Compute Halton indicies
  hl.out <- Ax*n.boxes[2]*gcd$s + Ay*n.boxes[1]*gcd$t
  hl.out <- hl.out %% prod(n.boxes)
  
  hl.out
  
}


#  pt.1 <- data.frame(x=0.43, y=0.64)
#  pt.2 <- data.frame(x=1.5/16, y=.5/9)
#  pt.3 <- data.frame(x= 0.4049793, y=0.281932)
#  pt.4 <- data.frame(x=0.4573888, y=0.6285714)
#  pt <- rbind(pt.1, pt.2, pt.3, pt.4)
#  
#  n.boxes <- c(16,9) 
# tmp<- halton.indicies.vector(pt, n.boxes) # should equal 70
# cat(paste("Halton index from vector routine:", tmp, "\n"))
# 
# tmp<- halton.indicies.CRT(pt, n.boxes) # should equal 70
# cat(paste("Halton index from CRT routine:", tmp, "\n"))

# # Plot Halton boxes and indicies to check
# b <- c(2,3)
# J <- c(4,2)
# hl.ind <- halton( 3*prod(n.boxes), 2,0 )
# plot(c(0,1),c(0,1),type="n",xaxt="n", yaxt="n")
# axis(1,at=(0:b[1]) / b[1])
# axis(2,at=(0:b[2]) / b[2])
# for( i in J[1]:1) abline(v=(0:b[1]^i)/b[1]^i, lwd=1, col=i)
# for( i in J[2]:1) abline(h=(0:b[2]^i)/b[2]^i, lwd=1, col=i)
# for( i in 1:prod(n.boxes)){
#   box.center <- (floor(n.boxes*hl.ind[i,]+.Machine$double.eps*10) + 1-.5)/n.boxes
#   text(box.center[1],box.center[2], i-1, adj=.5)  
# }
# points(pt$x, pt$y, col=6, pch=16, cex=.5)


# # Note, you cannot have a point exactly on the right or top edge. 
# tmp <- data.frame(x=(0:100)/101,y=.2)
# tmp.n <- 10000000
# tmp <- data.frame(x=runif(tmp.n), y=runif(tmp.n))
# n.boxes <- c(16,9)
# tmp.vec.time <- system.time( 
#   tmp.vec <- halton.indicies.vector(tmp, n.boxes) 
# )
# # tmp.crt.time <- system.time(
#   # tmp.crt <- halton.indicies.CRT(tmp, n.boxes)
# # )
# # tmp2 <- data.frame(row=1:length(tmp.vec), H.ind.vector=tmp.vec, H.ind.CRT=tmp.crt)
# # print(tmp2[10:60,])
# # cat("Number of non-equal indicies: ")
# # cat(sum(tmp2$H.ind.vector != tmp2$H.ind.CRT))
# # cat("\n")
# cat("Time for VECTOR routine to complete:\n")
# print(tmp.vec.time)
# # cat("Time for CRT routine to complete:\n")
# # print(tmp.crt.time)



# plot(c(0,1),c(0,1),type="n", xlim=c(0,1), ylim=c(.15,.25))
# for( i in J[1]:1) abline(v=(0:b[1]^i)/b[1]^i, lwd=1, col=i)
# for( i in J[2]:1) abline(h=(0:b[2]^i)/b[2]^i, lwd=1, col=i)
# for( i in 1:prod(n.boxes)){
#   box.center <- (floor(n.boxes*hl.ind[i,]+.Machine$double.eps*10) + 1-.5)/n.boxes
#   text(box.center[1],box.center[2], i-1, adj=.5)  
# }
# points(tmp$x, tmp$y, col=6, pch=16, cex=.5)
