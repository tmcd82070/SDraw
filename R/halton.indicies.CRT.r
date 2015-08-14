#' @export halton.indicies.CRT
#'  
#' @title Halton indicies by the Chinese Remainder Theorem (CRT)
#' 
#' @description Computes Halton indicies of 2-D points by solving the Chinese Remainder Theorem. 
#' This function is relatively slow, but it works for large problems. 
#' 
#' @param hl.coords nXD vector of coordinates for points 
#' @param n.boxes DX1 vector containing number of Halton boxes in each dimension.
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
#' This routine solves the Chinese Remainder Theorem to find Halton indicies.
#' This routine loops over the points in  \code{hl.coords}, and as such minimizes memory usage 
#' but sacrifices speed. For small problems,  see  \code{\link{F.halton.indicies.vector}}, 
#' which computes indicies by actually placing points in Halton boxes to find their indicies. 
#' 
#' @author Trent McDonald
#' 
#' 
#' @seealso \code{\link{F.halton.indicies.vector}}, \code{\link{F.halton.indicies}}
#' 
#' @examples 
#' pt <- data.frame(x=0.43, y=0.64)
#' n.boxes <- c(16,9) 
#' halton.indicies.vector(pt, n.boxes) # should equal 70
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
halton.indicies.CRT <- function(hl.coords, n.boxes, b=c(2,3), delta=c(1,1), ll.corner=c(0,0)){
  
  f.hal.index <- function(pt, n.boxes, b, J, xf, yf){
    # internal function to return Halton index for one point.  This is 
    # where CRT is solved. 
    x <- pt[1]
    y <- pt[2]
    b.x <- b[1]
    b.y <- b[2]
    nx <- n.boxes[1]
    ny <- n.boxes[2]
    jx <- J[1]
    jy <- J[2]
    
    print(c(x,y))
    
    # LL corner of point's box
    x.n <- floor(x*nx)
    y.n <- floor(y*ny)
    
    # Mod value in each dimension
    pows <- b.x^((jx-1):1)
    digits <- c(floor(x.n/pows), x.n %% b.x )
    Ax <- round(sum(rev(digits)*c(pows,1))) %% nx

    pows <- b.y^((jy-1):1)
    digits <- c(floor(y.n/pows), y.n %% b.y )
    Ay <- round(sum(rev(digits)*c(pows,1))) %% ny  # round() used to lop off fuzz

    print(c(Ax,Ay))
    
    # Solve CRT.  Find N s.t., N = Ax mod b.x and N= Ay mod b.y
    kx <- xf[xf[,2]==Ax ,1]
    ky <- yf[yf[,2]==Ay ,1]
    
    print(c(kx,ky))
    cat("--------------------------------------\n")
    
    N <- kx*ny + ky*nx
    N
  }
  
  # Scale points to [0,1]
  hl.coords <- t( t(as.matrix(hl.coords))/delta )
  
  # Compute J = exponents
  J <- log(n.boxes, b)
  if( !all( (J - trunc(J)) <= .Machine$double.eps * 1000) ){
    stop( "number of boxes in one or more dimensions is not an integer power of bases. Check n.boxes and b.")
  }
  
  # compute CRT lookup tables
  f.frac <- function(x){x - trunc(x)}
  
  tmp <- 0:(n.boxes[1]-1)
  tmp2 <- tmp*n.boxes[2]/n.boxes[1]
  x.factor <- cbind(tmp, round(f.frac(tmp2)*n.boxes[1]) )  # round used to lop off fuzz
  
  tmp <- 0:(n.boxes[2]-1)
  tmp2 <- tmp*n.boxes[1]/n.boxes[2]
  y.factor <- cbind(tmp, round(f.frac(tmp2)*n.boxes[2]) )

  # Loop over coordinates
  hl.out <- unlist(apply(hl.coords, 1, f.hal.index, n.boxes=n.boxes, b=b, J=J, xf=x.factor, yf=y.factor))
  

  hl.out
  
}

# tmp <- data.frame(x=(0:100)/100,y=.2)
# tmp <- F.halton.indicies.CRT(tmp, c(16,9))
# print(tmp)
# print(length(tmp))


