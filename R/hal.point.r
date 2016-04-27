#' @export hal.point
#' 
#' @title  Draws a Halton Lattice sample from a discrete (point) resource.
#' 
#' @description  Draws a Halton Lattice sample from a \code{SpatialPoints*} object. 
#' 
#' @details  \bold{A brief description of Halton Lattice sampling for points:} 
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
#'   \item \code{sampleID}: A unique identifier for every sample point that 
#'   encodes the HAL order.  \code{return[order(return$sampleID),]} will sort the 
#'   returned object in HAL order.  \code{sampleID}'s, in the HAL case, are not 
#'   consecutive. \code{sampleID}'s are the Halton indicies for the Halton boxes 
#'   containing the point, after adding random cycles to multiple points in 
#'   the same box (see \code{\link{halton.frame}}). 
#'    
#'   \item \code{geometryID}: The ID of the sampled point in \code{x} The 
#'   ID of points in \code{x} are \code{row.names(x)}. 
#'   
#'   \item Any attributes of the original points (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "point").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "HAL").
#'    \item \code{J}: Exponents of the bases used to form the lattice of 
#'    Halton boxes. This is either the input \code{J}, or the \code{J} vector
#'    computed by \code{\link{halton.indicies}}.  
#'    \item \code{bases}: Bases of the Halton sequence used to draw the sample. 
#'    \item \code{hl.box}: The bounding box around points in \code{x} used to 
#'    draw the sample.  See \code{\link{halton.indicies}}.
#'    \item \code{random.start}: The random start of the sample in the Halton
#'    frame.  The Halton frame is a list of all points sorted by their
#'    Halton indices, after adding random cycles to multiple points 
#'    in the same Halton box.  This number then is the 
#'    start of the sample in the frame.  
#'    It is a random number between 1 and the number of points in \code{x}.
#'    The sample consists of the 
#'    \code{n} consecutive units starting at \code{random.start} in 
#'    the sorted Halton frame. 
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
#'samp <- hal.point( 100, WA.cities )
#'
#'#   Different lattice topology
#'samp <- hal.point( 100, WA.cities, J=c(10,4))
#'   
#' 
hal.point <- function( x, n, J=NULL, bases=c(2,3)){

  if( !(inherits(x, "SpatialPoints"))  ) stop("Must call hal.point with a SpatialPoints* object.")
  
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
  
  
  # Reduce x to just it's coordinates, leave attributes of x for later
  N.frame <- length(x)
  x.coords <- data.frame(coordinates(x), geometryID=row.names(x))

    
  # Compute halton indicies of every point in x.  The Halton index is the index of the 
  # Halton box that the point falls in. 
  # Note: halton.indicies takes a SpatialPoints* object OR a data frame, so 
  # we could pass it x. but to save space I'm only passing the coordinates of 
  # x and a column that will allow us to select the sample
  hl.points <- halton.indicies(x.coords, J=J, bases=bases)
  
  # Make a Halton frame, which takes halton.index and adds cycles to points in same Halton box
  # This frame comes back sorted by halton order, ready to sample
  hl.points <- halton.frame( hl.points )
  
  # Draw sample from the frame
  m <- floor(runif(1, 0, N.frame)) # Integer 0,...,N.frame-1
  n <- min( n, N.frame )  # Can't take more than a census. 
  ind <- (((0:(n-1))+m) %% N.frame ) + 1  # Cycle the indicies around to start of frame if necessary
  
  samp <- hl.points[ind,]
  
  # Rearrange and rename columns
  coord.cols <- names(samp) %in% coordnames(x)
  samp.coords <- samp[,coord.cols]
  samp <- data.frame(samp[,!coord.cols], x@data[samp$geometryID,])

  names(samp)[names(samp)==attr(hl.points,"order.name")] <- "sampleID"

  samp.coords <- SpatialPoints(samp.coords, proj4string = CRS(proj4string(x)))
  samp <- SpatialPointsDataFrame(samp.coords, samp)
  
  # Add attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "point"
  attr(samp, "sample.type") <- "HAL"
  attr(samp,"J") <- attr(hl.points, "J")
  attr(samp,"bases") <- attr(hl.points, "bases")
  attr(samp,"hl.bbox") <- attr(hl.points, "hl.bbox")
  attr(samp,"random.start") <- m  # needed if we want more points from this frame,
      # actually, you need the randomized frame as well.  so you would have to 
      # reconstruct the whole frame to get more points.

  samp
  
  
}

# here!!! test hal.point with SpatialPoint object, SpatialPoint object without coordinate names
# check and fix row.names(samp), rename HaltonOrder as sampleID.