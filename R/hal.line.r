#' @export hal.line
#' 
#' @title  Draws a Halton Lattice sample from a linear (line) resource .
#' 
#' @description  Draws a Halton Lattice sample from a SpatialLines object, where SpatialLines
#' are defined in library 'sp'.
#' 
#' @details  A HAL sample is drawn from the union of all lines in \code{x} by first
#' discretizing lines with points spaced \code{pt.spacing} apart. After
#' discretizing the lines, discretization points are sampled using the HAL method
#' for points (see \code{\link{hal.point}}).
#' 
#' @param n Sample size.  Number of locations to draw from the set of all lines
#' contained in \code{x}.
#' @param x A SpatialLines or SpatialLinesDataFrame object. This object must
#' contain at least 1 line.  If it contains more than 1 line, the HAL sample is
#' drawn from the union of all lines.
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the smallest Halton boxes. There are \code{bases[1]^J[1]} vertical columns 
#' of Halton boxes over \code{x}'s bounding box, and \code{bases[2]^J[2]} 
#' horizontal rows of Halton boxes over the bounding box, for a total 
#' of \code{prod(bases^J)} total boxes.  The dimension of each box is 
#' \code{c(dx,dy)/(bases^J)}, where \code{c(dx,dy)} are the horizontal and 
#' vertical extents of \code{x}'s bounding box.  If \code{J=NULL} (the default),
#' \code{J} is choosen so that Halton boxes are as square as possible.
#' 
#' @param pt.spacing Desired spacing of points on lines prior to sampling via
#' HAL.  The first step in sampling is descritization which places
#' equaly-spaced points on all lines. Then, points are sampled using Halton sampling
#' (see \code{hal.point}) for points.  This parameter controls spacing of points during the
#' discretization of lines.  For example, specifying 50, and assuming
#' \code{x} is projected to UTM meters, means points will be placed every 50
#' meters along all lines in \code{x}. \code{x} should be projected before
#' sampling so that \code{pt.spacing} makes sense.  If \code{pt.spacing} is not
#' specified, 1000*\code{n} points will be placed along the lines during
#' descretization.
#' 
#' @param bases 2X1 vector of Halton bases.  These must be co-prime.
#' 
#' @param plot Logical indicating whether to plot \code{x} and selected points.  
#' @param plot.lattice Logical indicating whether to plot the Halton lattice used 
#' to draw the sample.  \code{plot.lattice = TRUE} produces same map 
#' as \code{plot=TRUE}, with the addition of the Halton lattice. 
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the HAL sample, in
#' the order they are to be visited.  A 'siteID' attribute is attached to each
#' point (in the embedded data frame) and gives the HAL ordering of the sample
#' (i.e., sort on 'siteID' to get proper HAL order).  In addition, if the input
#' object has an attached data frame (i.e., is a  \code{SpatialLinessDataFrame}), the
#' attrributes of the line on which each HAL point fell is attached in the
#' associated data frame. The number of the line in \code{x} on which each
#' point falls is an attribute of the output points.
#' 
#' @author Trent McDonald
#' @seealso \code{\link{bas.line}}, \code{\link{hal.point}}, \code{\link{hal.polygon}}, \code{\link{spsample}}
#' @keywords design survey
#' @examples
#' 
#'   #   Default sample of Hawaii coastline
#'   samp <- hal.line( 100, HI.coast )
#'   
#'   #   Frame spacing ~157 meters. Frame has ~10,000 points
#'   samp <- hal.line( 100, HI.coast, pt.spacing=157.1855, plot.lattice=TRUE )
#'   
#'   #   Different Halton lattice 
#'   samp <- hal.line( 100, HI.coast, J=c(5,3), pt.spacing=157.1855, plot.lattice=TRUE)
#' 
hal.line <- function( x, n, J=NULL, pt.spacing = NULL, bases=c(2,3),  
                      plot=TRUE, plot.lattice=FALSE){

  
  if( !inherits(x, "SpatialLines") ) stop("Must call hal.line with a SpatialLines* object.")
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  
  #   Discretize the line with many more points than needed for sample
  #this is code for Line
#   cc = coordinates(x)
#   lengths = LineLength(cc, longlat = FALSE, sum = FALSE)
#   if (any(abs(lengths) < .Machine$double.eps)) {
#     wl <- which(abs(lengths) < .Machine$double.eps)
#     cc <- cc[-(wl), ]
#     lengths <- lengths[-(wl)]
#   }  
#   csl = c(0, cumsum(lengths))
#   pts = ((1:n) - (1 - offset))/n * maxl
#   int = findInterval(pts, csl, all.inside = TRUE)
#   where = (pts - csl[int])/diff(csl)[int]
#   xy = cc[int, , drop = FALSE] + where * (cc[int + 1, , drop = FALSE] - cc[int, , drop = FALSE])
  
  #this is code for Lines
   L = x@lines
   lengths = sapply(L, function(x) LineLength(coordinates(x)[[1]]))
   if (sum(lengths) < .Machine$double.eps) 
     stop("Lines object of no length")
   
   # If frame spacing is not specified, set frame size to some multiple of n
   if(is.null(pt.spacing)){
     N <- 1000*n
   } else {
     N <- sum(lengths) / pt.spacing
   }
   nrs = round(lengths/sum(lengths) * N)
   ret = vector("list", sum(nrs > 0))
   j <- 1
   for (i in 1:length(L)) {
     if (nrs[i] > 0) {
       ret[[j]] = sp:::sample.Line(coordinates(L[[i]]), nrs[i], type = "regular")
       j <- j+1
     }
   }
   ret <- do.call(rbind, ret)
#  pt.frame <- sp::spsample( as(x, "SpatialLines"), N, type="regular" )

  # Now that we have points, we can draw a HAL point sample. 
  samp <- hal.point( n, ret, J, bases, plot, plot.lattice )

  samp

}


# ----- Some examples

#  samp<- hal.line(100, HI.coast)
#  plot(HI.coast)
#  points(samp)
