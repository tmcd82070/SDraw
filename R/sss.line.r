#' @export sss.line
#' 
#' @title Draw a Simple Systematic Sample (SSS) from a linear resource. 
#' 
#' @description Draws a systematic sample from a \code{SpatialLines*} object.  The 
#' \code{SpatialLines*} object represents a 2-dimensional line resource, such as a
#' river, highway, or coastline.  
#' 
#' @details This function is simply a wrapper for \code{spsample} in package 'sp'.
#' 
#' @param n Sample size.  Number of locations to draw from the set of all lines
#' contained in \code{x}.
#' @param x A \code{SpatialLiness} or \code{SpatialLinessDataFrame} object. 
#' This object
#' must contain at least 1 line.  If it contains more than 1 line, the sample
#' is drawn from the amalgomation of all lines.  Conceptually, the lines in \code{x} are 
#' "stretched" straight, laid end-to-end in order of appearence in \code{x}, 
#' and the systematic sample is drawn from this amalgomated line. 
#' Points on the amalgomated line are then mapped back to 2-dimensional 
#' space to fall on the lines in \code{x}. 
#' 
#' @param spacing Assuming, \code{n} is not given, this is the distance 
#' between sample points on the amalgomated line 
#' in \code{x}. For example, if \code{x} is projected 
#' in UTM coordinates and \code{spacing=100}, the returned sample has one point 
#' every 100 meters along the amalgomated line in \code{x}. Keep in mind that 
#' line i+1 in \code{x} may not touch the end of line i in \code{x}, and that 
#' lines in \code{x} may not be straight.  Thus, 2-dimensional distances between
#' sample points will not, in general, equal \code{spacing}.   
#' 
#' @return A SpatialPointsDataFrame containing locations in the SSS sample, in
#' order along the amalgomated line.  Those on line 1 appear first, those on line 2 
#' second, etc. 
#' 
#' @author Trent McDonald
#' @seealso \code{\link{sss.polygon}}, \code{\link{sss.point}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' 
#' HI.samp <- sss.line( 100, HI.coast )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' 
sss.line <- function(x, n, spacing=NA, random.start=TRUE){

if( !(inherits(x, "SpatialLines")) ) stop("Must call sss.line with a SpatialLines* object.")

#   This is easy, use spsample from the sp package.
#   Note: this is the only function that requires the Depends: sp statement in DESCRIPTION.
#   Without this call to spsample, you can Import sp, rather than Depend it.
samp <- spsample( x, n, type="regular" )

#   Make answer into a SpatialPointsDataFrame to be consistent with other SDraw routines
#   It would be nice to transfer over the attributes of x, but x is a line, 
#   and the over() function does not take points over lines (makes sense, no area to lines). 
#   And, I cannot figure out a nice way to buffer the lines and use over.

samp <- SpatialPointsDataFrame( samp,  data=data.frame(siteID=1:length(samp)) )

samp

}
