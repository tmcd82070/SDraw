#' @export grts.line
#'  
#' @title Draw a equi-probable GRTS sample from a linear (line) resource.
#' 
#' 
#' @description Draws an equi-probable unstratified Generalized Random
#' Tesselation Stratified (GRTS) sample from a \code{SpatialLines} object
#' 
#' @details This is a wrapper for the \code{grts} function in package \code{spsurvey}.
#' This should make calling \code{grts} for relatively simple frames easier. For more 
#' complicated frames and designs (e.g., variable probability, stratification), call 
#' \code{grts} directly. 
#' 
#' @param n Sample size.  The number of 'sample' points to draw from \code{shp}
#' @param shp A \code{SpatialLines} or \code{SpatialLinesDataFrame} object.
#' @param over.n Over-sample size.  The number of 'over-sample' points to draw
#' from \code{shp}.  The actual number of points drawn from \code{shp} is
#' \code{n + over.n}.
#'
#' @return A \code{SpatialPointsDataFrame} containing locations in the GRTS sample, in
#' order they are to be visited.  A \code{siteID} attribute is attached to each
#' point (in the embedded data frame) and gives the GRTS ordering of the sample
#' (i.e., sort on 'siteID' to get proper GRTS order).  In addition, if the
#' input lines have attributes (i.e., \code{shp} is a \code{SpatialLinesDataFrame}), 
#' attributes of the original lines onto which sample points fell are included as attributes 
#' of the sample points.
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
#' @examples
#' 
#' 
#' #   Draw sample
#' \dontrun{
#' # Because spsurvey::grts sometimes stops when it cannot create a temporary 
#' # shapefile. 
#' HI_sample <- grts.line(100,HI.coast,100)
#' 
#' #   Plot
#' plot( HI.coast )
#' 
#' # Plot 'sample' locations
#' plot( HI_sample[ HI_sample$pointType == "Sample", ], pch=16, add=TRUE, col="red" )  
#' 
#' # Plot 'over sample' locations
#' plot( HI_sample[ HI_sample$pointType == "OverSample", ], pch=1, add=TRUE, col="blue" )  
#' }
#' 
#'
grts.line <- function( n, shp, over.n=0 ){
  
  grts.equi(n, shp, over.n)
  
}