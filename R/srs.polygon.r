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
#'   \item \code{siteID}: A unique identifier for every sample point.   
#'   
#'   \item \code{geometryID}: The ID of the polygon in \code{x} which each 
#'   sample point falls.  The 
#'   ID of polygons in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original polygons (in \code{x}). 
#' }
#'
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
#' points( WA.samp )
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
A <- rgeos::gArea(shp)  # If shp is not projected, this generates a warning. 
#A <- sum(unlist(lapply( x@polygons, function(x){ x@area}))) #this always works, but I don't know about holes

# Compute number of points to generate to get approx n inside shape
n.big <- (A.bb/A)*n

# Generate more random points than we need
m.x <- runif( 200 )   # burn 200 random numbers.   I have doubts about the randomness of the first few numbers of R's runif
m.x <- runif( n.big, bb[1,1], bb[1,2] )
m.y <- runif( n.big, bb[2,1], bb[2,2] )


#   Make sample into a SpatialPoints object
samp <- SpatialPoints( data.frame(x=m.x, y=m.y), proj4string=CRS(proj4string(x)) )

#   Clip to x
x@data <- data.frame( geometryID=row.names(geometry(x)), data.frame(x),  zzz=1 )   #  make sure data frame has at least one numeric column
tmp <- over( samp, x )
keep <- !is.na(tmp$zzz)
tmp <- tmp[,!(names(tmp) %in% c("zzz"))] 
samp@data <- data.frame( tmp )
samp <- samp[ keep, ]

loop again and make sur you have n points

#   Add spacing as attribute
attr(grd, "spacing.m") <- delta
attr(grd, "rand.dir") <- -theta
attr(grd, "rand.shift") <- c(m.x, m.y)
attr(grd, "triangular") <- triangular

grd

}
