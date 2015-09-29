#'
#'  @name spsample
#'  
#'  @aliases spsample spsample.SpatialLines spsample.SpatialPoints spsample.SpatialPolygons
#'  
#'  @title field is in SDraw.line.  I could not get roxyen to use @title in this file
#'  
#'  
#'  @description Draw sample (point locations) from SpatialPoints, SpatialLines, 
#'  and SpatialPolygons. This is extends the generic \code{spsample} function of 
#'  package \code{sp} to draw balanced samples. 
#'  
#'  
#'  @param x A spatial object.  Methods for \code{SpatialPoints}, \code{SpatialLines}, and 
#'  \code{SpatialPolygons} are in \code{SDraw}. Methods for other classes are 
#'  in package \code{sp}.  
#'  @param n Desired sample size.  Some \code{type}'s of samples are fixed-size, in which case 
#'  exactly \code{n} points are returned.  Other \code{type}'s are variable-size, and
#'  this number is the approximate sample size. 
#'  @param type Character, naming the type of sample to draw. Valid \code{type}'s are:
#'  \itemize{
#'    \item \code{"HAL"}  : HAlton Lattice sampling (Robertson et al., (Forthcoming))
#'    \item \code{"BAS"}  : Balanced Acceptance Sampling (Robertson et al., 2013)
#'    \item \code{"SSS"}  : Simple Systematic (grid) Sampling, with extended capabilities
#'    \item \code{"GRTS"} : Generalized Random Tesselation Stratified sampling (Stevens and Olsen, 2004) 
#'    \item \code{"random"} : Simple random sampling (via methods in \code{sp})
#'    \item \code{"regular"} : Simple Systematic (grid) Sampling (via methods in \code{sp})
#'    \item \code{"stratified"} : Stratified sampling with one point in each in each "cell" 
#'    (via methods in \code{sp})
#'    \item \code{"nonaligned"} : Nonaliged systematic (grid) sampling (random x and y coordinates)
#'    (via methods in \code{sp})
#'    \item \code{"hexagonal"} : Hexagonal lattice sampling (via methods in \code{sp})
#'    \item \code{"clustered"} : Cluster sampling (via methods in \code{sp})
#'    \code{"Fibonacci"} : Fibonacci sampling on the sphere (via methods in \code{sp})
#'  } 
#'  @param ... Optional arguments passed to underlying sample type method.  See NOTES.
#'  
#'  @details This is a S4 generic method for types \code{SpatialPoints?}, \code{SpatialLines?}, 
#'  and \code{SpatialPolygons?} objects.  ? means these objects can come with an attached
#'  data frame containing attributes. 
#'  
#'  @return A \code{SpatialPointsDataFrame} object.  At a minimum, the data frame 
#'  embedded in the \code{SpatialPoints} object contains a column named \code{siteID} which 
#'  numbers the points. If \code{x} is a \code{Spatial?DataFrame}, the return's  data
#'  frame contains all attributes of \code{x} evaluated at the locations of the sample points.
#'  
#'  @note The follwing sample types are fixed-size (return exactly \code{n} points):
#'  \code{HAL, BAS, GRTS, random}.  The rest are variable-size. 
#'  
#'  See documentation for \code{hal.?}, \code{bas.?}, \code{sss.?}, and \code{grts.?} for additional 
#'  parameters passed via \code{...}.  
#'  
#'  See additional notes on \code{sp} methods in \code{help(spsample, package=sp)}.
#'  
#'  
#'  @author Trent McDonald
#'  
#'  @references 
#'  Robertson, B.L., J. A. Brown,  T. L. McDonald, and P. Jaksons (2013) "BAS: 
#'  Balanced Acceptance Sampling of Natural Resources", Biometrics, v69, p. 776-784.
#'  
#'  Stevens D. L. Jr. and A. R. Olsen (2004) "Spatially Balanced Sampling of Natural Resources", 
#'  Journal of the American Statistical Association, v99, p. 262-278.
#'  
#'  @seealso 
#'  \code{\link{bas.polygon}}, \code{\link{bas.line}}, \code{\link{bas.point}},
#' \code{\link{hal.polygon}}, \code{\link{hal.line}}, \code{\link{hal.point}}, 
#' \code{\link{sss.line}}, \code{\link{sss.polygon}},
#' \code{\link{grts.polygon}}, \code{\link{grts.line}}, \code{\link{grts.point}}
#'    
#'  @examples 
#'  WA.sample <- spsample(WA, 100, "HAL")
#'  WA.sample <- spsample(WA, 100, "SSS", spacing=c(1,2))
#'  

NULL



