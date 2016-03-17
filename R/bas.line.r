#' @export bas.line
#' @title Draws a Balanced Asseptance Sample (BAS) from a linear resource (line).
#' 
#' @description Draws a BAS sample from a SpatialLines object.
#' 
#' @details A BAS sample is drawn from the union of all lines in \code{x} by first
#' discretizing lines with a dense systematic sample of points.  The number of
#' points in this dense systematic sample is \code{n*n.pixel.factor}.  After
#' discretizing the line, points on the lines are sampled using the BAS method
#' for points (see \code{\link{bas.point}}).  The BAS method for points places
#' a small square (pixel) around each point and samples the set of all squares
#' using the BAS method for polygons (see \code{\link{bas.polygon}}).  The BAS
#' method of polygons selects Halton points until \code{n} points are located
#' inside the squares that surround points.  When a square contains a Halton
#' point, the sample location is the center of the square, which falls on the
#' line.
#' 
#' @param n Sample size.  Number of locations to draw from the set of all lines
#' contained in \code{x}.
#' @param x A SpatialLines or SpatialLinesDataFrame object. This object must
#' contain at least 1 line.  If it contains more than 1 line, the BAS sample is
#' drawn from the union of all lines.
#' @param n.pixel.factor Scalar controling the number of underlying pixels to
#' create on the lines.  Number of pixels created on the line is
#' \code{n*n.pixel.factor}, so this number can grow quickly.  On average, this
#' is the number of unselected points between each selected point.  See
#' Details.
#' @return A \code{SpatialPointsDataFrame} containing locations in the BAS 
#' sample, in BAS
#' order.  \code{siteID} in the 
#' embedded data frame  gives the BAS ordering of the sample
#' (i.e., BAS ordering can be restored by sorting on \code{siteID}.  
#' In addition, if the input
#' object has an attached data frame (i.e., is a \code{SpatialLinessDataFrame}), 
#' attrributes of the line on which each BAS point falls is included in the
#' output object's embedded data frame. The number of the line 
#' in \code{x} on which each
#' point falls is also included in the attribute data frame.
#' @author Trent McDonald
#' @seealso \code{\link{bas.line}}, \code{\link{bas.polygon}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' #   Draw sample of Hawaii coastline
#' #   This takes approximately 60 seconds to run
#' \dontrun{
#' samp <- bas.line( 100, HI.coast )
#' plot(HI.coast, col=rainbow(length(HI.coast)))
#' points( samp, pch=16 )
#' }
#' 
#' 
bas.line <- function(x, n, n.pixel.factor=5){

#   Function to draw a bas sample from a shapefile
#   containing linear 1-d features.


if( !inherits(x, "SpatialLines") ) stop("Must call bas.line with a SpatialLines* object.")

##   Check for projected coordinates and convert if necessary.
#ps <- proj4string(x)
#if( regexpr("proj=longlat", ps) > 0 ){
#    #   We have a lat-long system  - Issue a warning
#    warning( paste("SDRAW: Lat-Long coordinate system found. Recommend conversion of \n",
#    "original spatial lines to UTM and re-drawing sample. Use of Lat-Long will slightly reduce\n", 
#    "spatial balance when sampling long lines."))
#}

#   Discretize the line with n.pixel.factor more points than needed for sample
pt.frame <- sss.line( x, n*n.pixel.factor )

#   Sample as points
samp <- bas.point( pt.frame, n )

samp

}
