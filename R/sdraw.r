#' @title Draw samples from spatial objects.
#'
#' @description Draw samples (point locations) from \code{SpatialPoints}, \code{SpatialLines}, 
#' \code{SpatialPolygons}, and the \code{*DataFrame} varieties of each. 
#'  
#'  
#' @param x A spatial object.  Methods are implemented for \code{SpatialPoints}, 
#' \code{SpatialPointsDataFrame}, \code{SpatialLines}, \code{SpatialLinesDataFrame}, 
#' \code{SpatialPolygons}, and 
#' \code{SpatialPolygonsDataFrame} objects. 
#' @param n Desired sample size.  Some \code{type}'s of samples are fixed-size (see NOTES), 
#' in which case 
#' exactly \code{n} points are returned.  Other \code{type}'s are variable-size, 
#' and this number is the expected sample size (i.e., average over many repititions). 
#' @param type Character, naming the type of sample to draw. Valid \code{type}'s are:
#' \itemize{
#' \item \code{"HAL"}  : HAlton Lattice sampling (Robertson et al., (Forthcoming)) (fixed-size)
#' \item \code{"BAS"}  : Balanced Acceptance Sampling (Robertson et al., 2013) (fixed-size)
#' \item \code{"SSS"}  : Simple Systematic (grid) Sampling, with random start and orientation (variable-size)
#' \item \code{"GRTS"} : Generalized Random Tesselation Stratified sampling 
#'      (Stevens and Olsen, 2004) (fixed-size)
#' \item \code{"SRS"}  : Simple Random Sampling (fixed-size)
#' }
#'
#' @param ... Optional arguments passed to underlying sample type method.  See NOTES.
#'  
#' @details This is a S4 generic method for types \code{SpatialPoints?}, \code{SpatialLines?}, 
#' and \code{SpatialPolygons?} objects.  
#'  
#' \code{HAL, BAS, GRTS, SRS} are fixed-size designs (return exactly \code{n} points).
#' \code{SSS} is variable-sized. 
#'
#' See documentation for \code{hal.?}, \code{bas.?}, \code{sss.?}, and \code{grts.?} for additional 
#' parameters passed via \code{...}.  
#'  
#'
#' @return A \code{SpatialPointsDataFrame} object.  At a minimum, the data frame 
#' embedded in the \code{SpatialPoints} object contains a column named \code{siteID} which 
#' numbers the points. If \code{x} is a \code{Spatial?DataFrame}, the return's  data
#' frame contains all attributes of \code{x} evaluated at the locations of the sample points.
#'
#' @author Trent McDonald
#'
#' @references 
#'  Robertson, B.L., J. A. Brown,  T. L. McDonald, and P. Jaksons (2013) "BAS: 
#'  Balanced Acceptance Sampling of Natural Resources", Biometrics, v69, p. 776-784.
#'  
#'  Stevens D. L. Jr. and A. R. Olsen (2004) "Spatially Balanced Sampling of Natural Resources", 
#'  Journal of the American Statistical Association, v99, p. 262-278.
#'  
#' @seealso 
#'  \code{\link{bas.polygon}}, \code{\link{bas.line}}, \code{\link{bas.point}},
#' \code{\link{hal.polygon}}, \code{\link{hal.line}}, \code{\link{hal.point}}, 
#' \code{\link{sss.polygon}}, \code{\link{sss.line}},  \code{\link{sss.point}},
#' \code{\link{grts.polygon}}, \code{\link{grts.line}}, \code{\link{grts.point}}
#'    
#' @examples 
#'  WA.sample <- sdraw(WA, 100, "HAL")
#'  WA.sample <- sdraw(WA, 100, "SSS", spacing=c(1,2))
#'  
#' @name sdraw
#'  
#' @aliases sdraw sdraw.SpatialLines sdraw.SpatialPoints sdraw.SpatialPolygons
#'  
#'  
sdraw <- function(x, ...) UseMethod("sdraw")

#' @export
#' @docType methods
#' @rdname sdraw
setMethod("sdraw", c(x="SpatialPolygons"), sdraw.SpatialPolygons)

#' @rdname sdraw
setMethod("sdraw", c(x="SpatialLines"), sdraw.SpatialLines)

#' @rdname sdraw
setMethod("sdraw", c(x="SpatialPoints"), sdraw.SpatialPoints)

