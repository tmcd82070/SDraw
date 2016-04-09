#' @export bas.polygon
#' 
#' @title Draws a Balanced Asseptance Sample (BAS) from an area resource (polygons).
#' 
#' @description Draws a BAS sample from a SpatialPolygons* object
#' 
#' @details A BAS sample is drawn from the union of all polygons in \code{x} by
#' enclosing all polygons in a bounding square and selecting a randomized
#' Halton sequence of points from the bounding square.  Points falling outside
#' all polygons are discarded until exactly \code{n} locations are selected
#' inside the polygons.
#' 
#' The sampling frame for routine is infinite and contains all (infintesibly 
#' small) points in the union of polygons in \code{x}. 
#' 
#' @param n Sample size.  Number of locations to draw from the union of all
#' polygons contained in \code{x}.
#' 
#' @param x A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object. 
#' This object
#' must contain at least 1 polygon.  If it contains more than 1 polygon, the
#' BAS sample is drawn from the union of all polygons.
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the BAS sample, 
#' in BAS order.
#'  Attributes of the sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  This 
#'   encodes the BAS order.  \code{return[order(return$sampleID),]} will sort the 
#'   returned object in BAS order.

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
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "BAS").
#'    \item \code{random.start}: The start for the randomized Halton sequence 
#'    that produced the sample.  This is a vector of length 2 of random 
#'    uniform numbers between 0 and 10e7.  This routine ensures that this index 
#'    in the randomized Halton sequence falls inside a polygon of interest.  i.e., 
#'    that \code{halton(1,2,random.start)} scaled by the bounding box 
#'    lies inside a polygon of \code{x}.  \code{halton(1,2,random.start+i)}, for 
#'    \code{i} > 0, is not guarenteed to fall inside a polygon of \code{x}
#'    when scaled by the bounding box. 
#' }
#' 
#' @references 
#' 
#' Robertson, B.L., J. A. Brown, T. L. McDonald, and P. Jaksons
#' (2013) "BAS: Balanced Acceptance Sampling of Natural Resources", Biometrics,
#' v69, p. 776-784.
#' 
#' @author Trent McDonald
#' @seealso \code{\link{bas.line}}, \code{\link{bas.point}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' #   Draw sample
#' WA_sample <- bas.polygon(WA, 100)  
#' 
#' #   Plot
#' plot( WA )
#' 
#' # Plot first 100 sample locations
#' points( WA_sample[ WA_sample$siteID <= 100, ], pch=16 ) 
#' 
#' # Plot second 100 locations 
#' points( WA_sample[ WA_sample$siteID >  100, ], pch=1 )  
#' 
#' 
bas.polygon <- function( x, n ){

#   Check n
if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
}

#   Find bounding box around everything
bb <- bbox( x )

#   Find area of all polygons
# Could call gArea at this point.  But, If x is not projected, 
# gArea generates a warning.  Could suppress it, but, I've chosen 
# to compute my own area, which accounts for holes and does not 
# care whether x is projected.  I think i can drop dependency on rgeos 
# if I do this.
# Here is the call to gArea
#area <- suppressWarnings((rgeos::gArea(x)))  

# Here is the call to my routine.
area <- polygonArea(x)

#   Find fraction of the square Halton box covered by the polygons
p <- min(1, area / max(diff(t(bb)))^2 )

#   Maximum number for random start.  Random start is uniform on integers between 1 and this number. 
max.u <- 10e7


my.dim <- 2 # number of dimensions we are sampling


#   Make sure there is a non-missing attribute associated with each polygon in x.
#   This is because over() extracts attributes of x, and missingness is used 
#   to flag which points are outside a polygon.
if( inherits(x, "SpatialPolygonsDataFrame") ){
  #   x has a data frame
  df <- data.frame( sampleID=1:length(x), geometryID=row.names(x), data.frame(x) )
} else {
  df <- data.frame( sampleID=1:length(x), geometryID=row.names(x),  row.names=row.names(x) )
}

x <- SpatialPolygonsDataFrame( x, data=df )

crs.obj <- CRS(proj4string(x))

#   Draw initial random start, but make sure the first point is inside the study area.
q <- 1 - p
z <- qnorm(0.90)
n.init <- (1 / p) + (q*z*z/(2*p)) + (z / p)*sqrt(z*z*q*q/4 + q*1)  # term in sqrt is >0 because we have control on all terms
n.init <- ceiling(n.init)
repeat{
  m <- ceiling(max.u * runif( n.init ))
  halt.samp <- matrix(NA, n.init, my.dim)
  for(i in 1:n.init){
    halt.samp[i,] <- halton( 1, my.dim, m[i] )
  }
  
  #   Convert from [0,1] to a square box covering [bb]
  halt.samp <- bb[,"min"] + t(halt.samp) * rep( max(diff(t(bb))), 2)
  halt.samp <- t(halt.samp)
  
  halt.pts <- SpatialPointsDataFrame(halt.samp, data=data.frame(sampleID=1:n.init),
                                     proj4string=crs.obj )
  
  in.poly <- over( halt.pts, x )
  
  keep <- !is.na( in.poly$sampleID )
  
  if(any(keep)) break
}


# Keep first (or any) that are in the polygon
m <- m[keep][1]
halt.pts <- halt.pts[keep,][1,]

#   Take initial number of Halton numbers that is approximately correct
#   This is number of samples to take to be Alpha% sure that we get n 
#   points in the study area.  99% of time this loop runs once.
#   At this point, halt.pts has one point in it.
q <- 1 - p
z <- qnorm(0.99)
halt.start <- m  # save for attributes later
repeat{
  n.init <- (n / p) + (q*z*z/(2*p)) + (z / p)*sqrt(z*z*q*q/4 + q*n)  # term in sqrt is >0 because we have control on all terms
  n.init <- ceiling(n.init)
  halt.samp <- halton( n.init, my.dim, m+1 )
  
  #   Convert from [0,1] to a square box covering [bb]
  halt.samp <- bb[,"min"] + t(halt.samp) * rep( max(diff(t(bb))), 2)
  halt.samp <- t(halt.samp)
  
  #   Check which are in the polygon, after first converting halt.samp to SpatialPoints
  #   And adding to points from previous iteration
  #   sampleID in this data frame gets overwritten below when assign to @data

  halt.pts2 <- SpatialPointsDataFrame(halt.samp, proj4string=crs.obj, data=data.frame(sampleID=1:nrow(halt.samp)) )
  halt.pts <- rbind(halt.pts, halt.pts2)
  

  in.poly <- over( halt.pts, x )
  
  #   Reject the points outside the polygon, and attach other attributes if present
  keep <- !is.na( in.poly$sampleID )  # in.poly$sampleID is row num of polygon in x


  halt.pts@data <- data.frame( in.poly )
  halt.pts <- halt.pts[ keep, ]

  #   The way we computed n.init, there should be more points in halt.pts than we need. Keep the initial ones.
  if( length(halt.pts) >= n ){
      halt.pts <- halt.pts[1:n,]
      halt.pts$sampleID <- 1:n   # renumber the site ID's because some (those outside polygon) were tossed above
      break
  } else {
      n <- n - length(halt.pts)
      m <- m + length(halt.pts)  # place in Halton sequence to start next iter
  }

}  

attr(halt.pts, "frame") <- deparse(substitute(x))
attr(halt.pts, "frame.type") <- "polygon"
attr(halt.pts, "sample.type") <- "BAS"
attr(halt.pts, "random.start") <- halt.start

halt.pts

}
