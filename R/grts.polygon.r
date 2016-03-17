#' @export grts.polygon
#'  
#' @title Draw a equi-probable GRTS sample from an area (polygon) resource.
#' 
#' @description Draws an equi-probable unstratified Generalized Random
#' Tesselation Stratified (GRTS) sample from a \code{SpatialPolygons*} object
#' 
#' @details This is a wrapper for the \code{grts} function in package \code{spsurvey}.
#' This simplifies calling \code{grts} when equi-probable samples are 
#' desired. It extends the allowable input frame types to \code{SpatialPolygons} objects 
#' (i.e., no attributes),
#' rather than just \code{SpatialPolygonsDataFrame} objects. For more 
#' complicated designs (e.g., variable probability, stratification), call 
#' \code{grts} directly. 
#' 
#' @param n Sample size.  The number of 'sample' points to draw from \code{x}
#' @param x A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object.
#' @param over.n Over-sample size.  The number of 'over-sample' points to draw
#' from \code{x}.  The actual number of points drawn from \code{x} is
#' \code{n + over.n}.
#'
#' @return A \code{SpatialPointsDataFrame} containing locations in the GRTS sample, in
#' order they are to be visited.  Attributes of the sample points (in the embedded data frame) are 
#' as follows: 
#' \itemize{
#'   \item \code{siteID}: Gives the GRTS ordering of the sample.  The output object
#'   comes pre-sorted in GRTS order (i.e., sorted by \code{siteID}).  
#'   If the sample  becomes un-GRTS-ordered, resort 
#'   by \code{siteID} (i.e., \code{samp <- samp[order(samp$siteID),]}). 
#'   \item \code{pointType}: A string identifying regular sample points (\code{pointType=="Sample"})
#'   and over-sample points (\code{pointType=="OverSample"}).
#'   \item \code{geometryID}: The ID of the point in \code{x} which was sampled.  The 
#'   ID of polygons in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original points (in \code{x}). 
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{grts.line}}, \code{\link{grts.polygon}}, \code{\link{hal.polygon}}, 
#' \code{\link{bas.polygon}}, \code{\link{sdraw}}
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
#' #   Draw sample
#' WA.sample <- grts.polygon(WA,100,50)
#' 
#' #   Plot
#' plot( WA )
#' 
#' # Plot 'sample' locations
#' plot( WA.sample[ WA.sample$pointType == "Sample", ], pch=16, add=TRUE, col="red" )  
#' 
#' # Plot 'over sample' locations
#' plot( WA.sample[ WA.sample$pointType == "OverSample", ], pch=1, add=TRUE, col="blue" )  
#' 
#' 
#'
grts.polygon <- function( x, n, over.n=0 ){
  
  if( !inherits(x, "SpatialPolygons")) stop("Must call grts.polygon with a SpatialPolygons object.")
  
  grts.equi(x, n, over.n)
  
}