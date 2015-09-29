#' @export hal.line
#' 
#' @title  Draws a Halton Lattice sample from a linear (line) resource .
#' 
#' @description  Draws a Halton Lattice sample from a SpatialLines object, where SpatialLines
#' are defined in library 'sp'.
#' 
#' @details  A HAL sample is drawn from the union of all lines in \code{shp} by first
#' discretizing lines with points spaced \code{pt.spacing} apart. After
#' discretizing the lines, discretization points are sampled using the HAL method
#' for points (see \code{\link{hal.point}}).
#' 
#' @param n Sample size.  Number of locations to draw from the set of all lines
#' contained in \code{shp}.
#' @param shp A SpatialLines or SpatialLinesDataFrame object. This object must
#' contain at least 1 line.  If it contains more than 1 line, the HAL sample is
#' drawn from the union of all lines.
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the smallest Halton boxes. There are \code{bases[1]^J[1]} vertical columns 
#' of Halton boxes over \code{shp}'s bounding box, and \code{bases[2]^J[2]} 
#' horizontal rows of Halton boxes over the bounding box, for a total 
#' of \code{prod(bases^J)} total boxes.  The dimension of each box is 
#' \code{c(dx,dy)/(bases^J)}, where \code{c(dx,dy)} are the horizontal and 
#' vertical extents of \code{shp}'s bounding box.  If \code{J=NULL} (the default),
#' \code{J} is choosen so that Halton boxes are as square as possible.
#' @param pt.spacing Desired spacing of points on lines prior to sampling via
#' HAL.  The first step in sampling is to descritize lines by placing
#' equaly-spaced points on them, then sampling points using Halton sampling
#' (see \code{hal.point}).  This parameter controls spacing of points in the
#' discretization of lines.  For example, specifying 50, and assuming
#' \code{shp} is projected to UTM meters, means points will be placed every 50
#' meters along all lines in \code{shp}. \code{shp} should be projected before
#' sampling so that \code{pt.spacing} makes sense.  If \code{pt.spacing} is not
#' specified, 1000*\code{n} points will be placed along the lines during
#' descretization.
#' @param bases 2X1 vector of Halton bases.  These must be co-prime.
#' @return A \code{SpatialPointsDataFrame} containing locations in the HAL sample, in
#' the order they are to be visited.  A 'siteID' attribute is attached to each
#' point (in the embedded data frame) and gives the HAL ordering of the sample
#' (i.e., sort on 'siteID' to get proper HAL order).  In addition, if the input
#' object has an attached data frame (i.e., is a  \code{SpatialLinessDataFrame}), the
#' attrributes of the line on which each HAL point fell is attached in the
#' associated data frame. The number of the line in \code{shp} on which each
#' point fell is an attribute of the output points.
#' @author Trent McDonald
#' @seealso \code{\link{bas.line}}, \code{\link{hal.point}}, \code{\link{hal.polygon}}, \code{\link{spsample}}
#' @keywords design survey
#' @examples
#' 
#'   #   Draw sample of Hawaii coastline
#'   #   This takes approximately 30 seconds to run
#'   samp <- hal.line( 100, HI.coast )
#'   plot(HI.coast, col=rainbow(length(HI.coast)))
#'   points( samp, pch=16 )
#'   
#' 
hal.line <- function( n, shp, J=NULL, pt.spacing = NULL, bases=c(2,3)){

  
  if( regexpr("Lines", class(shp)) < 0 ) stop("Must call hal.line with a SpatialLinesX object.")
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # If lattice is not specified, set frame size to some multiple of n
  if(is.null(pt.spacing)){
    N <- 1000*n
  } else {
    N <- NULL
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
#   L = x@Lines
#   lengths = sapply(L, function(x) LineLength(x@coords))
#   if (sum(lengths) < .Machine$double.eps) 
#     stop("Lines object of no length")
#   nrs = round(lengths/sum(lengths) * n)
#   ret = vector("list", sum(nrs > 0))
#   j = 1
#   for (i in 1:length(L)) {
#     if (nrs[i] > 0) {
#       ret[[j]] = sample.Line(L[[i]], nrs[i], type = type, 
#                              offset = offset, ...)
#       j = j + 1
#     }
#   }
#   do.call(rbind, ret)
  pt.frame <- spsample( as(shp, "Lines"), N, type="regular" )
  
  # Now that we have points, we can draw a HAL point sample. 
  samp <- hal.point( n, pt.frame, J, bases )
  
  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
