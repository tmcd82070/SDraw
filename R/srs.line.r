#' @export srs.line
#' 
#' @title Draw a Simple Random Sample (SRS) from a linear resource. 
#' 
#' @description Draws a simple random sample from a \code{SpatialLines*} object.  The 
#' \code{SpatialLines*} object represents a 2-dimensional line resource, such as a
#' river, highway, or coastline.  
#' 
#' @details If \code{x} contains multiple lines, the lines are amalgomated before
#' sampling.   Conceptually, under amalgomation the lines in \code{x} are 
#' "stretched" straight and laid end-to-end in order of appearence in \code{x}.
#' The simple random sample is then drawn from the amalgomated line. 
#' Once drawn from the 1-D amalgomated line, sample pointsare mapped back 
#' to 2-dimensional space to fall on the lines in \code{x}. 
#' 
#' Note that the line is not discretized prior to sampling.  The sample points 
#' are selected from the set of continuous lines that contain 
#' an infinite number of points (up to machine precision anyway). 
#' 
#' 
#' @param n Sample size.  Number of points to draw from the set of all lines
#' contained in \code{x}.  
#' 
#' @param x A \code{SpatialLines} or \code{SpatialLinesDataFrame} object. 
#' This object must contain at least 1 line.  
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the SRS sample, in
#' order along the amalgomated line.  Those on line 1 appear first, those on line 2 
#' second, etc. Attributes of the sample points (in the 
#' embedded data frame) are 
#' as follows: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  
#'   \code{sampleID} starts with 1 at the first point and 
#'   increments by one for each.  \code{sampleID} orders 
#'   sample points along the amalgomated line.
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
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SRS").
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{srs.polygon}}, \code{\link{srs.point}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # Draw fixed number of equi-distant points
#' HI.samp <- srs.line( HI.coast, 100 )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' # Inspect attributes of points with HI.samp@data
#' 
srs.line <- function(x, n){

  if( !(inherits(x, "SpatialLines")) ) stop("Must call srs.line with a SpatialLines* object.")

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

  # Draw the l.out points along parameterized line ("l","x","y")
  tot.len <- mline[nrow(mline),"l"]
  l.out <- runif( n, 0, tot.len ) 

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
  attr(samp, "sample.type") <- "SRS"

  samp
}

# # test with SpatialPoints object (no data frame)
# tmp <-  srs.line(as(HI.coast, "SpatialLines"), 100)
# plot(HI.coast)
# points(tmp, pch=16)
# 
# tmp <-  srs.line(HI.coast, 100)
# plot(HI.coast)
# points(tmp, pch=16)

# # test on lat-long coordinate system
#  HI.ll <- spTransform(HI.coast, CRS("+init=epsg:4326"))
#  tmp <- sss.line(HI.ll, 50)
