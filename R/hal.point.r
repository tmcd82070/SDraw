#' @export hal.point
#' 
#' @title  Draws a Halton Lattice sample from a discrete (point) resource.
#' 
#' @description  Draws a Halton Lattice sample from a \code{SpatialPoints} object. 
#' 
#' @details  \emph{Brief description of Halton Lattice sampling for points:} 
#' Given a set of Halton Lattice parameters \code{J} and \code{bases},
#' a Halton lattice is constructed over the bounding box of the input points.  
#' This results in \code{prod(bases^J)} Halton boxes on the bounding box. 
#' The Halton index of all boxes is computed and assigned to points that lie 
#' in each box.  Points that lie in the same Halton box are randomly assigned 
#' unique Halton cycle numbers. This separates points in the same Halton box by
#' at least \code{prod(bases^J)} units when indicies are mapped to the real line. 
#' Finally, a random number between 1 and the largest Halton (index+cycle) is 
#' drawn, and the next \code{n} units in the mapped real numbers are taken as 
#' the sample, restarting from the beginning if necessary.
#' 
#' @param n Sample size.  Number of locations to draw from the set of points
#' contained in \code{shp}.
#' @param shp A SpatialPointss or SpatialPointssDataFrame object. This object must
#' contain at least 1 point.  
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the lowest level of Halton boxes. If \code{J=NULL} (the default),
#' \code{J} is choosen so that Halton boxes are as square as possible.
#' @param bases 2X1 vector of Halton bases.  These must be co-prime.

#' @return A SpatialPointsDataFrame containing locations in the HAL sample, in
#' the order they are to be visited.  A 'siteID' attribute is attached to each
#' point (in the embedded data frame) and gives the HAL ordering of the sample
#' (i.e., sort on 'siteID' to get proper HAL order).  In addition, if the input
#' object has an attached data frame (i.e., is a  \code{SpatialPointsDataFrame}), the
#' attrributes of the line on which each HAL point fell is attached in the
#' associated data frame. The number of the line in \code{shp} on which each
#' point falls also appears in the attribute data frame.
#' @author Trent McDonald
#' @seealso \code{\link{hal.line}}, \code{\link{hal.polygon}}, \code{\link{spsample}}, \code{\link{bas.point}}
#' @keywords design survey
#' @examples
#' 
#'   #   Draw sample of Hawaii coastline
#'   #   This takes approximately 30 seconds to run
#'   data(WA.cities)
#'   samp <- hal.point( 100, WA.cities )
#'   plot(WA.cities)
#'   points( samp, pch=16, col="red" )
#'   
#' 
hal.point <- function( n, shp, J=NULL, bases=c(2,3)){

  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # If lattice is not specified, set frame size to some multiple of n
  if(is.null(J)){
    N <- 100*n

    bb <- bbox( shp )
    
    D <- nrow( bb )   # number of dimensions
    
    delta <- apply( bb, 1, diff ) 
    
    # Set default values of J so Halton boxes are as close to squares as possible
    n.boxes <- rep(NA,D)  # n boxes in each dimension
    for( i in 1:D ){
      n.boxes[i] <- ((delta[i]^(D-1))/prod(delta[-i]) * N)^(1/D)
    }
    
    # compute J which gives something close to n
    J <- round( log(n.boxes)/log(bases) )
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0
  }
  
  attr(shp,"J") <- J
  attr(shp,"bases") <- bases
  if( is.null( attr(shp, "hl.box" )) ){
    bb <- bbox(shp)
    # Because Halton boxes are closed on bottom and left, open on top and right, we 
    # need to add a tiny amount to top and right just in case any points are exactly 
    # on the bounding box top or right boundaries. If we did not do this, a point exactly 
    # on the top or right boundary would technically be outside all Halton boxes. 
    delta <- apply( bb, 1, diff ) 
    bb[,2] <- bb[,2] + 0.0001*delta
    attr(shp,"hl.bbox") <- bb
  }
  
  # Compute halton indicies of every point in shp.  The Halton index is the index of the 
  # Halton box that the point falls in. 
  hl.points <- halton.indicies(shp)
  
  # Make a Halton frame, which takes halton.index and adds cycles to points in same Halton box
  # This frame comes back sorted by halton order, ready to sample
  hl.points <- halton.frame( hl.points )
  
  # Draw sample from the frame
  m <- runif(200)    # burn 200 random numbers from R's routine
  N.frame <- nrow(hl.points)
  m <- floor(runif(1, 0, N.frame))
  n <- min( n, N.frame )  # Can't take more than a census. 
  ind <- ((((1:n)+m)-1) %%  N.frame)+1   # Cycle the indicies around to start of frame if necessary
  
  
  samp <- hl.points[ind,]
  
  # Add attributes
  attr(samp,"J") <- attr(hl.points, "J")
  attr(samp,"eta") <- attr(hl.points, "eta")
  attr(samp,"bases") <- attr(hl.points, "bases")
  attr(samp,"hl.bbox") <- attr(hl.points, "hl.bbox")
  attr(samp,"triangular") <- attr(hl.points, "trianglular")
  attr(samp,"index.name") <- attr(hl.points,"index.name")
  attr(samp,"order.name") <- attr(hl.points,"order.name")
  attr(samp,"m") <- m  # needed if we want more points from this frame
  
  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
