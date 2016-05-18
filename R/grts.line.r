#' @export grts.line
#'  
#' @title Draw a equi-probable GRTS sample from a linear (line) resource.
#' 
#' 
#' @description Draws an equi-probable unstratified Generalized Random
#' Tessellation Stratified (GRTS) sample from a \code{SpatialLines*} object
#' 
#' @details This is a wrapper for the \code{grts} function in package \code{spsurvey}.
#' This simplifies calling \code{grts} when equi-probable samples are 
#' desired. It extends the allowable input frame types to \code{SpatialLines} objects 
#' (i.e., no attributes),
#' rather than just \code{SpatialLinesDataFrame} objects. For more 
#' complicated designs (e.g., variable probability, stratification), call 
#' \code{grts} directly. 
#' 
#' @param n Sample size.  The number of sample points to draw from \code{x}
#' @param x A \code{SpatialLines} or \code{SpatialLinesDataFrame} object. 
#' @param over.n Over-sample size.  The number of 'over-sample' points to draw
#' from \code{x}.  The actual number of points drawn from \code{x} is
#' \code{n + over.n}.
#'
#' @return A \code{SpatialPointsDataFrame} containing locations in the GRTS sample, in
#' order they are to be visited.  Attributes of the sample points (in the embedded data frame) are 
#' as follows: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for points in the sample. 
#'   This encodes the GRTS ordering of the sample.  The output object
#'   comes pre-sorted in GRTS order.  
#'   If the sample becomes un-GRTS-ordered, resort 
#'   by \code{sampleID} (i.e., \code{samp <- samp[order(samp$sampleID),]}). 
#'   \item \code{pointType}: A string identifying regular sample points (\code{pointType=="Sample"})
#'   and over-sample points (\code{pointType=="OverSample"}).
#'   \item \code{geometryID}: The ID of the line in \code{x} onto which sample points fall.
#'   The ID's of lines in \code{x} are \code{row.names(geometry(x))}.
#'   \item Any attributes of the original lines (in \code{x}) onto which sample points 
#'   fall. 
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{grts.line}}, \code{\link{grts.polygon}}, \code{\link{hal.line}}
#' , \code{\link{spsample}}
#' 
#' @references Stevens, D. L. and A. R. Olsen (1999). Spatially restricted
#' surveys over time for aquatic resources. Journal of Agricultural,
#' Biological, and Environmental Statistics 4 (4), 415-428.
#' 
#' Stevens, D. L. and A. R. Olsen (2004). Spatially balanced sampling of
#' natural resources. Journal of the American Statistical Association 99,
#' 262-278.
#' @keywords design survey
#
# Sometimes, spsurvey::grts stops because it cannot create a temporary 
# shapefile. This seems to be machine specific.  If this happens, 
# surround these examples with \dontrun
# 
#' @examples
#' 
#' # Draw sample
#' HI.sample <- grts.line(HI.coast,100,50)
#' 
#' # Plot
#' plot( HI.coast )
#' 
#' # Plot 'sample' locations
#' plot( HI.sample[ HI.sample$pointType == "Sample", ], pch=16, add=TRUE, col="red" )  
#' 
#' # Plot 'over sample' locations
#' plot( HI.sample[ HI.sample$pointType == "OverSample", ], pch=1, add=TRUE, col="blue" )  
#' 
#' 
#'
grts.line <- function( x, n, over.n=0 ){
  
  if( !inherits(x, "SpatialLines")) stop("Must call grts.line with a SpatialLines object.")
 
  grts.equi(x, n, over.n)

}


