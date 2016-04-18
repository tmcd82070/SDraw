#' @export hal.point
#' 
#' @title  Draws a Halton Lattice sample from a discrete (point) resource.
#' 
#' @description  Draws a Halton Lattice sample from a \code{SpatialPoints*} object. 
#' 
#' @details  \emph{A brief description of Halton Lattice sampling for points:} 
#' Given a set of Halton Lattice parameters \code{J} and \code{bases},
#' a lattice of Halton boxes is constructed over the bounding box of the input points.  
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
#' contained in \code{x}.
#' 
#' @param x A \code{SpatialPoints} or \code{SpatialPointsDataFrame} object. 
#' This object must contain at least 1 point.  
#' 
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the smallest Halton boxes. There are \code{bases[1]^J[1]} vertical columns 
#' of Halton boxes over \code{x}'s bounding box, and \code{bases[2]^J[2]} 
#' horizontal rows of Halton boxes over the bounding box, for a total 
#' of \code{prod(bases^J)} boxes.  The dimension of each box is 
#' \code{c(dx,dy)/(bases^J)}, where \code{c(dx,dy)} are the horizontal and 
#' vertical extents of \code{x}'s bounding box.  If \code{J=NULL} (the default),
#' \code{J} is choosen so that Halton boxes are as square as possible.
#' 
#' @param bases 2X1 vector of Halton bases.  These must be co-prime.
#' 
#' @param plot Logical indicating whether to plot \code{x} and selected points.  
#' 
#' @param plot.lattice Logical indicating whether to plot the Halton lattice used 
#' to draw the sample.  \code{plot.lattice = TRUE} produces same map 
#' as \code{plot=TRUE}, with the addition of the Halton lattice.  
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the HAL sample, 
#' in HAL order.
#' Attributes of the sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  This 
#'   encodes the HAL order.  \code{return[order(return$sampleID),]} will sort the 
#'   returned object in HAL order.
#'   
#'   \item \code{geometryID}: The ID of the sampled point in \code{x} The 
#'   ID of points in \code{x} are \code{row.names(x)}. 
#'   \item Any attributes of the original points (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "point").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "HAL").
#'    \item \code{random.start}: The random seed of the Halton lattice sample. 
#'    This is the random number between 1 and the largest Halton (index+cycle) 
#'    that initiated the sample.  The sample consists of the 
#'    \code{n} consecutive units starting at \code{random.start} in 
#'    the Halton order. 
#' }
#' 
#' 
#' @author Trent McDonald
#' 
#' @seealso \code{\link{hal.line}}, \code{\link{hal.polygon}}, \code{\link{sdraw}}, 
#' \code{\link{bas.point}}
#' 
#' @keywords design survey
#' @examples
#' 
#'#   Draw sample of Hawaii coastline
#'#   This takes approximately 30 seconds to run
#'samp <- hal.point( 100, WA.cities, plot.lattice=TRUE )
#'
#'#   Different lattice topology
#'samp <- hal.point( 100, WA.cities, J=c(10,4), plot.lattice=TRUE)
#'   
#' 
hal.point <- function( x, n, J=NULL, bases=c(2,3)){

  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # If lattice is not specified, set approximate number of boxes
  # to number of points in frame, so approximately one per box.
  if(is.null(J)){
    N <- length( x )

    bb <- bbox( x )
    
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
  

  # Compute halton indicies of every point in x.  The Halton index is the index of the 
  # Halton box that the point falls in. 
  hl.points <- halton.indicies(x, J=J, bases=bases)
  
  # Make a Halton frame, which takes halton.index and adds cycles to points in same Halton box
  # This frame comes back sorted by halton order, ready to sample
  hl.points <- halton.frame( hl.points )
  
  # Draw sample from the frame
  N.frame <- nrow(hl.points)
  m <- floor(runif(1, 0, N.frame)) # Integer 0,...,N.frame-1
  n <- min( n, N.frame )  # Can't take more than a census. 
  ind <- (((0:(n-1))+m) %% N.frame ) + 1  # Cycle the indicies around to start of frame if necessary
  
  samp <- hl.points[ind,]
  
  # Add attributes
  attr(samp,"J") <- attr(hl.points, "J")
  attr(samp,"bases") <- attr(hl.points, "bases")
  attr(samp,"hl.bbox") <- attr(hl.points, "hl.bbox")
  attr(samp,"index.name") <- attr(hl.points, "index.name")
  attr(samp,"random.start") <- m  # needed if we want more points from this frame

  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
