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
#'   \item If \code{x} inherits from \code{SpatialPointsDataFrame}, 
#'   returned points have attribute \code{geometryID} -- the ID (=\code{row.names(x)}) of 
#'   the sampled point. 
#'   \item Any attributes (columns) associated with the input points (rows).
#' }
#'
#' 
#' Additional attributes of the output object are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "point").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SSS").
#'    \item \code{sample.spacing}: The spacing between sample points along the 
#'    line segment 0 to n. This is equal to (N/n)
#'    \item \code{random.start}: The random start for the systematic sample.
#' }
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

  if( !(inherits(x, "SpatialPoints")) ) stop("Must call sss.point with a SpatialPoints* object.")

  N <- nrow(x)
  
  # check input parameters
  if( !missing(n)){
    if (!is.finite(n) || n < 1) return(NULL)
    if( n > N ) return(x)
  }
  

  # Construct index vector
  k <- N / n
  m <- runif(1,0,1)
  m <- seq(m, n)
  f <- seq(N/n, n, by=N/n)
  
  frame <- data.frame(x)
  
  # Main code ========================================================
  # Get all coordinates from all lines "back to back" in a matrix
  mline.ids <- merge.lines(x)
  mline <- mline.ids$geometry
  mline.ids <- mline.ids$IDs
  
  #print(data.frame(mline,mline.ids))

  # Figure out l.out sequence along parameterized line ("l","x","y")
  tot.len <- mline[nrow(mline),"l"]
  if(!missing(n)){
    # figure out spacing. Want n equally space points along 
    # entire length of line.
    spacing <- tot.len / n
  } 
  l.out <- seq(0,tot.len-spacing,by=spacing)

  if( random.start ){
    m <- 
    l.out <- l.out + runif(1, 0, spacing)
  }

  # Extract or compute points on the parameterized line, and indices (tt)
  x.out <- aprox( mline[,"l"], mline[,3], l.out)
  y.out <- aprox( mline[,"l"], mline[,4], l.out)
  t.out <- aprox( mline[,"l"], mline[,"t"], l.out)
  
  # Extract line ID's at each point
  geoID.out <- mline.ids[ceiling(t.out)]
    

  # output ===========================================================
  crds <- data.frame(x.out,y.out)
  names(crds)<- dimnames(mline)[[2]][3:4]
  row.names(crds) <- 1:length(x.out)
  samp<-SpatialPoints( crds, proj4string = CRS(proj4string(x)) )
  
  if( inherits(x, "SpatialLinesDataFrame") ){
    # x has attributes, extract them at the points
    df <- data.frame(x)[geoID.out, ]
    df <- data.frame( siteID=1:length(x.out), geometryID=geoID.out, df)
    row.names(df) <- 1:length(x.out)
  } else {
    df <- data.frame( siteID=1:length(x.out), geometryID=geoID.out )
    row.names(df) <- 1:length(x.out)
  }
  samp <- SpatialPointsDataFrame(samp, df, proj4string = CRS(proj4string(x)), match.ID = TRUE)

  #   Add additional attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "line"
  attr(samp, "sample.type") <- "SSS"
  attr(samp, "sample.spacing") <- spacing
  attr(samp, "random.start") <- random.start
  
  samp
}

# # test with SpatialPoints object (no data frame)
# tmp <-  sss.line(as(HI.coast, "SpatialLines"), 100)
# plot(HI.coast)
# points(tmp, pch=16)
# 
# # Test with a SpatialLinesDataFrame
# tmp <- sss.line(HI.coast, spacing=20000)
# plot(HI.coast[1,])  # HI.coast[1] is the big island only
# points(tmp,pch=16)
# 
# # tmp <- sss.line(Sl, 10)
# # plot(Sl)
# # points(tmp)
# 
# library(sp)
# Sll <- SpatialLinesDataFrame(Sl, data.frame(color=c("red","blue"), elev=c(1000, 848), row.names = c("a","b")))
# tmp <- sss.line(Sll, 10)
# plot(Sll)
# points(tmp)
# print(data.frame(tmp))

# # test on lat-long coordinate system
# HI.ll <- spTransform(HI.coast, CRS("+init=epsg:4326"))
# tmp <- sss.line(HI.ll, 50)
