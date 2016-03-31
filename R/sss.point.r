#' @export sss.point
#' 
#' @title Draw a Simple Systematic Sample (SSS) from a point resource or finite population frame. 
#' 
#' @description Draw a systematic sample from a \code{SpatialPoints*} object or a \code{data.frame}.   
#' \code{SpatialPoints*} objects can represent point resouces in 2-dimensional space, such as towns, event locations, 
#' or grid cell centers.
#' 
#' @details The points in \code{x} are systematically sampled in the order
#' they appear. That is, the sampling frame (i.e., \code{data.frame(x)}) is 
#' \emph{not} re-ordered prior to sampling.  Each row in the frame represents 
#' a point or sample unit, and rows are sampled systematically starting with row 1.
#' To draw a systematic sample across the range of an attribute, say attribute \eqn{y}, 
#' sort \code{x} by \eqn{y} prior to calling this routine (e.g,. \code{sss.point( x[order(x$y),], n )}). 
#' 
#' 
#' This routine draws \emph{fixed size} systematic samples. Many 
#' systematic sampling procedure produce variable size samples. Conceptually, the sample 
#' procedure is:
#' \enumerate{ 
#'  \item Each sample unit (= row of sample frame) is associated with a line segment. Assuming there
#' are \eqn{N} units in the frame (\eqn{N} = \code{nrow(x)}), each line segment has length 
#' \eqn{n/N}, where \eqn{n} is the input desired sample size.  
#'  \item Line segments are placed end-to-end, starting at 0, in the order in which their associated
#'  unit appears in the frame. 
#'  \item To start the systematic sample, the routine choses a random number between 0 and 1. 
#' Let this random number be \eqn{m}.
#'  \item The sample unit associated with the line segment containing the numbers \eqn{m + i} for 
#' \eqn{i} = 0,1,...,(\eqn{n-1}), are selected for the sample.       
#' }
#' 
#' @param n Sample size.  Number of points to draw from the set of all points
#' in \code{x}. If \code{n} exceeds the number of units (= number of rows in \code{data.frame(x)}), 
#' a census is taken (i.e., \code{x} is returned). 
#' 
#' @param x A \code{SpatialLines}, \code{SpatialLinesDataFrame}, or \code{data.frame} object. 
#' 
#' @return If input \code{x} inherits from a the \code{SpatialPointsDataFrame} class, a
#' \code{SpatialPointsDataFrame} object containing locations in the sample is returned. 
#' If input \code{x} is a \code{data.frame}, a \code{data.frame} is returned. 
#' Attributes of the returned sample points are: 
#' \itemize{
#'   \item \code{siteID}: A unique identifier for every sample point.  
#'   \code{siteID} starts with 1 at the first point and 
#'   increments by one for each.  
#'   \item If \code{x} inherits from \code{SpatialPoints}, 
#'   returned points have attribute \code{geometryID} -- the ID (=\code{row.names(x)}) of 
#'   the sampled point. 
#'   \item Any attributes (columns) associated with the input points (rows).
#' }
#' Additional attributes of the output object are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame (i.e., \code{x}).
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "point").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SSS").
#'    \item \code{random.start}: The random start for the systematic sample.
#' }
#' Using these additional attributes, one could reconstruct the sample.
#' 
#' @author Trent McDonald
#' @seealso \code{\link{sss.polygon}}, \code{\link{sss.line}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # Draw systematic sample across range of population
#' WA.samp <- sss.point( WA.cities[order(WA.cities$POP_2010),], 100 )   
#' plot( WA.cities )
#' points( WA.samp, col="red", pch=16 )
#' 
#' # Draw systematic sample from data frame
#' df <- data.frame( a=1:100, b=runif(100) )
#' samp <- sss.point( df, 5 )   
#' 
#' # One way to draw a simple random sample: randomly sort frame.
#' samp <- sss.point( df[order(df$b),] 5 )
#' 
sss.point <- function(x, n){

  if( !(inherits(x, "SpatialPoints")) & !(inherits(x, "data.frame")) ) stop("Must call sss.point with a SpatialPoints* or data.frame object.")

  if( inherits(x, "SpatialPoints")){
    N<- length(x)
  } else {
    N <- nrow(x)
  }

  # check input parameters
  if( !missing(n)){
    if (!is.finite(n) || n < 1) return(NULL)
    if( n > N ) return(x)
  }
  

  # Construct index vector
  m <- runif(1,0,1)
  m <- seq(m, n)
  f <- c(0, seq(n/N, n, by=n/N))
  ind <- rep(NA, n)
  for(i in 1:n){
    ind[i] <- which( (f[-(N+1)] < m[i]) & (m[i] <= f[-1]) )
  }
  
  # Extract sample
  samp <- x[ind,]

  # Turn into correct output object type.
  if( inherits(x, "SpatialPointsDataFrame") ){
    samp@data <- data.frame(siteID=1:n, geometryID=row.names(samp), samp@data )    
  } else if( inherits(x, "data.frame")){
    samp <- data.frame(siteID=1:n,  samp )    
  } else { # SpatialPoints only
     df <- data.frame(siteID=1:n, geometryID=row.names(samp) )
     samp <- SpatialPointsDataFrame(samp, df, proj4string = CRS(proj4string(samp)))
  }
  

  #   Add additional attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "point"
  attr(samp, "sample.type") <- "SSS"
  attr(samp, "random.start") <- m[1]
  
  samp
}

# # test with SpatialPoints object (no data frame)
# tmp <-  sss.point(as(WA.cities, "SpatialPoints"), 10)
# plot(WA.cities)
# points(tmp, pch=16, col="red")
# 
# # Test with a SpatialLinesDataFrame
# tmp <- sss.point(WA.cities, 100)
# plot(WA.cities)
# points(tmp, pch=16, col="red")
# 
# tmp <- data.frame(a=1:300, b=runif(300))
# tmp <- sss.point( tmp, 50)