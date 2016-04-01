#' @export srs.point
#' 
#' @title Draw a Simple Random Sample (SRS) from a point resource or finite population frame. 
#' 
#' @description Draw a systematic sample from a \code{SpatialPoints*} object or a \code{data.frame}.   
#' \code{SpatialPoints*} objects can represent point resouces in 2-dimensional space, such as towns, event locations, 
#' or grid cell centers.
#' 
#' @details When \code{x} is a data frame, the simple random sample is drawn from the rows.  That is, 
#' each row is viewed as a sample unit. 
#' 
#' This draws equi-probable sample.  First order inclusion probabilities are n/N for all units.
#' 
#' @param n Sample size.  Number of points or rows to draw from \code{x}. 
#' If \code{n} exceeds the number of units (= number of rows in \code{data.frame(x)}), 
#' a census is taken (i.e., \code{x} is returned). 
#' 
#' @param x A \code{SpatialLines}, \code{SpatialLinesDataFrame}, or \code{data.frame} object. 
#' 
#' @return If input \code{x} inherits from a the \code{SpatialPoints} class, a
#' \code{SpatialPointsDataFrame} object containing locations and attributes in the sample is returned. 
#' If input \code{x} is a \code{data.frame}, a \code{data.frame} is returned. 
#' Attributes of the returned sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  
#'   \code{sampleID} starts with 1 at the first point and 
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
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SRS").
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{srs.polygon}}, \code{\link{srs.line}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # Draw systematic sample across range of population
#' WA.samp <- srs.point( WA.cities, 100 )   
#' plot( WA.cities )
#' points( WA.samp, col="red", pch=16 )
#' 
#' # Draw systematic sample from data frame
#' df <- data.frame( a=1:100, b=runif(100) )
#' samp <- srs.point( df, 5 )   
#' 
srs.point <- function(x, n){

  if( !(inherits(x, "SpatialPoints")) & !(inherits(x, "data.frame")) ) stop("Must call srs.point with a SpatialPoints* or data.frame object.")

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
  ind <- ceiling( runif(n,0,N) )

  # Extract sample
  samp <- x[ind,]

  # Turn into correct output object type.
  if( inherits(x, "SpatialPointsDataFrame") ){
    samp@data <- data.frame(sampleID=1:n, geometryID=row.names(samp), samp@data )    
  } else if( inherits(x, "data.frame")){
    samp <- data.frame(sampleID=1:n,  samp )    
  } else { # SpatialPoints only
     df <- data.frame(sampleID=1:n, geometryID=row.names(samp) )
     samp <- SpatialPointsDataFrame(samp, df, proj4string = CRS(proj4string(samp)))
  }
  

  #   Add additional attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "point"
  attr(samp, "sample.type") <- "SRS"

  samp
}

# # test with SpatialPoints object (no data frame)
# tmp <-  srs.point(as(WA.cities, "SpatialPoints"), 10)
# plot(WA.cities)
# points(tmp, pch=16, col="red")
# 
# # Test with a SpatialLinesDataFrame
# tmp <- srs.point(WA.cities, 100)
# plot(WA.cities)
# points(tmp, pch=16, col="red")
# 
# tmp <- data.frame(a=1:300, b=runif(300))
# tmp <- srs.point( tmp, 50)