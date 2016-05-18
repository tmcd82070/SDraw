#' @export sss.line
#' 
#' @title Draw a Simple Systematic Sample (SSS) from a linear resource. 
#' 
#' @description Draws a systematic sample from a \code{SpatialLines*} object.  The 
#' \code{SpatialLines*} object represents a 2-dimensional line resource, such as a
#' river, highway, or coastline.  
#' 
#' @details If \code{x} contains multiple lines, the lines are amalgamated before
#' sampling.   Conceptually, under amalgamation the lines in \code{x} are 
#' "stretched" straight and laid end-to-end in order of appearance in \code{x}.
#' The simple systematic sample is then drawn from the amalgamated line. 
#' Finally, sample points on the amalgamated line are mapped back to 2-dimensional 
#' space to fall on the lines in \code{x}. 
#' 
#' Note that spacing between sample points is enforced on the amalgamated 
#' line, and may not look correct if the lines loop back on themselves. 
#' For example, consider a line tracing a circle.  The spacing between 
#' the first and last sample point along the circle will not be the 
#' prescribed \code{spacing} because the circle starts between them. 
#' Spacing of all other points (2 to n-1) will be as requested. 
#'
#' 
#' @param n Sample size.  Number of points to draw from the set of all lines
#' contained in \code{x}.  Specification of \code{n} takes precedence 
#' over specification of \code{spacing}.
#' 
#' @param x A \code{SpatialLines} or \code{SpatialLinesDataFrame} object. 
#' This object must contain at least 1 line.  
#' 
#' @param spacing Assuming, \code{n} is not given, this is the distance 
#' between sample points on the amalgamated line 
#' in \code{x}. For example, if \code{x} is projected 
#' in UTM coordinates and \code{spacing=100}, the returned sample has one point 
#' every 100 meters along the amalgamated line in \code{x}. Keep in mind that the start
#' of line i+1 in \code{x} may not coincide with the end of line i in \code{x}, and that 
#' lines in \code{x} may not be straight.  Thus, 2-dimensional distances between
#' sample points will not, in general, equal \code{spacing}.   
#' 
#' @param random.start Whether to start the sequence of points at a 
#' random place.  If \code{TRUE}, a random uniform variate is selected 
#' between 0 and either \code{spacing} or (length/\code{n}) and the first 
#' location is placed at that location along the line.  Subsequent points occur
#' every \code{spacing} units along the lines.  If \code{random.start==FALSE}, 
#' the first sample point occurs at 0 (first vertex of the lines).
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the SSS sample, in
#' order along the amalgamated line.  Those on line 1 appear first, those on line 2 
#' second, etc. Attributes of the sample points (in the 
#' embedded data frame) are 
#' as follows: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  
#'   \code{sampleID} starts with 1 at the first point and 
#'   increments by one for each.  \code{sampleID} orders 
#'   sample points along the amalgamated line.
#'   \item \code{geometryID}: The ID of the lines object in \code{x} on which each 
#'   sample point falls.  The 
#'   ID of lines in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original lines (in \code{x}) on which each sample 
#'   point falls.
#' }
#'
#' 
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "line").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SSS").
#'    \item \code{sample.spacing}: The spacing between sample points along the 
#'    amalgamated line. This is the input \code{spacing} parameter if specified,
#'    or is computed as (length/n) if \code{n} is specified.
#'    \item \code{random.start}: The random start of the systematic sample.  NA 
#'    corresponds to no random start.
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{sss.polygon}}, \code{\link{sss.point}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # Draw fixed number of equi-distant points
#' HI.samp <- sss.line( HI.coast, 100 )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' # Draw points every 20 km along Hawaii's coastline
#' HI.samp <- sss.line( HI.coast, spacing=20000 )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' # Inspect attributes of points with HI.samp@data
#' 
sss.line <- function(x, n, spacing, random.start=TRUE){

  if( !(inherits(x, "SpatialLines")) ) stop("Must call sss.line with a SpatialLines* object.")

  if(!missing(n) & !missing(spacing)) warning("n and spacing both specified in sss.line.  n is being used.")

  # check input parameters
  if( !missing(n)){
    if (!is.finite(n) || n < 1) return(NULL)
  }
  

  
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
    m <- runif(1, 0, spacing)
    l.out <- l.out + m
  } else {
    m <- NA
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
    df <- data.frame( sampleID=1:length(x.out), geometryID=geoID.out, df)
    row.names(df) <- 1:length(x.out)
  } else {
    df <- data.frame( sampleID=1:length(x.out), geometryID=geoID.out )
    row.names(df) <- 1:length(x.out)
  }
  samp <- SpatialPointsDataFrame(samp, df, proj4string = CRS(proj4string(x)), match.ID = TRUE)

  #   Add additional attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "line"
  attr(samp, "sample.type") <- "SSS"
  attr(samp, "sample.spacing") <- spacing
  attr(samp, "random.start") <- m
  
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
