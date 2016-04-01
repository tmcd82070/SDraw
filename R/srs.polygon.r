#' @export srs.polygon
#' 
#' @title  Draws a Simple Random Sample (SRS) from an area resource (polygons).
#' 
#' @description Draws a simple random sample from a \code{SpatialPolygons} or 
#' \code{SpatialPolygonsDataFrame} object.  
#' 
#' @details 
#' 
#' @param n Sample size.  Number of locations to draw from the union of all
#' polygons contained in \code{x}.
#' 
#' @param x A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} 
#' object. This object
#' must contain at least 1 polygon.  If it contains more than 1 polygon, the
#' SRS sample is drawn from the union of all polygons.  Holes are respected.
#' 
#' 
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the SRS sample, 
#' in arbitrary order.  Attributes of the sample points (in the 
#' embedded data frame) are 
#' as follows: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.   
#'   
#'   \item \code{geometryID}: The ID of the polygon in \code{x} which each 
#'   sample point falls.  The 
#'   ID of polygons in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original polygons (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "polygon").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SRS").
#' }
#'    
#'    
#'     
#' @author Trent McDonald
#' @seealso \code{\link{bas.polygon}}, \code{\link{sss.polygon}}, 
#' \code{\link{hal.polygon}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # A square grid oriented east-west
#' WA.samp <- srs.polygon( WA, 100 )   
#' plot( WA )
#' points( WA.samp, pch=16 )
#' 
#' 
srs.polygon <- function( x, n ){

  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }
    
  # Bounding box of shapefile
  bb <- bbox( x )
  
  # Area of bounding box
  A.bb <- diff(bb[1,])*diff(bb[2,])
  
  #   Find area of all polygons
  A <- rgeos::gArea(x)  # If x is not projected, this generates a warning. 
  #A <- sum(unlist(lapply( x@polygons, function(x){ x@area}))) #this always works, but I don't know about holes
  
  # Compute number of points to generate to get approx n inside shape
  n.big <- ceiling((A.bb/A)*n)
  
  # burn 200 random numbers.   I have doubts about the randomness of the first few numbers of R's runif
  m.x <- runif( 200 )   
  
  #  make sure data frame has at least one numeric column
  x@data <- data.frame( sampleID=1:length(x), geometryID=row.names(geometry(x)), data.frame(x),  zzz=1 )   
  
  # Loop until we get enough samples inside the polygons
  samp.pts.x <- samp.pts.y <- NULL
  samp.attr <- NULL
  repeat{
    # Generate random points
    m.x <- runif( n.big, bb[1,1], bb[1,2] )
    m.y <- runif( n.big, bb[2,1], bb[2,2] )
  
    #   Make sample into a SpatialPoints object
    tmp.samp <- SpatialPoints( data.frame(x=m.x, y=m.y), proj4string=CRS(proj4string(x)) )
  
    #   Find points in x, and extract attributes of x at those points
    tmp <- over( tmp.samp, x )
    keep <- !is.na(tmp$zzz)
  
    samp.pts.x <- c(samp.pts.x, m.x[keep])
    samp.pts.y <- c(samp.pts.y, m.y[keep])
    samp.attr <- rbind(samp.attr, data.frame(tmp[keep,!(names(tmp) %in% c("zzz"))] ))
    
    if( length(samp.pts.x)  >= n ){
      # We have more than enough inside polygons.  Keep only first n.
      samp.pts.x <- samp.pts.x[1:n]
      samp.pts.y <- samp.pts.y[1:n]
      samp.attr <- samp.attr[1:n,]
      break    
    } else {
      n.big <- ceiling((A.bb/A)*(n - length(samp.pts.x)))
    }
  }
  
  # Make into a SpatialPointsDataFrame
  samp <- SpatialPointsDataFrame(data.frame(x=samp.pts.x, y=samp.pts.y), 
                                 samp.attr, 
                                 proj4string=CRS(proj4string(x)))
  
  #   Add additional attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "polygon"
  attr(samp, "sample.type") <- "SRS"
  
  samp

}
