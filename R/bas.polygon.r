#' @export bas.polygon
#' 
#' @title Draws a Balanced Acceptance Sample (BAS) from an area resource (polygons).
#' 
#' @description Draws a BAS sample from a SpatialPolygons* object
#' 
#' @details A BAS sample is drawn from the union of all polygons in \code{x} by
#' enclosing all polygons in a bounding square and selecting a randomized
#' Halton sequence of points from the bounding square.  Points falling outside
#' all polygons are discarded until exactly \code{n} locations are selected
#' inside the polygons.
#' 
#' The sampling frame for routine is infinite and contains all (infinitesimally 
#' small) points in the union of polygons in \code{x}. 
#' 
#' @param n Sample size.  Number of locations to draw from the union of all
#' polygons contained in \code{x}.
#' 
#' @param x A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object. 
#' This object
#' must contain at least 1 polygon.  If it contains more than 1 polygon, the
#' BAS sample is drawn from the union of all.
#' 
#' @return A \code{SDrawSample} object, which is a  
#' \code{SpatialPointsDataFrame} object containing points in the BAS sample, 
#' in BAS order.
#'  Attributes of the sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  This 
#'   encodes the BAS order.  \code{return[order(return$sampleID),]} will sort the 
#'   returned object in BAS order.
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
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "BAS").
#'    \item \code{random.start}: The random seed of the random-start Halton sequence 
#'    that produced the sample.  This is a vector of length 2 whose elements are 
#'    random integers between 0 and \code{\link{maxU}}. 
#'    This routine ensures that the point
#'    associated with this index 
#'    falls inside a polygon of \code{x}.  i.e., 
#'    that \code{halton(1,2,random.start)} scaled by a square bounding box
#'    (see attribute \code{bas.bbox} below)
#'    lies inside a polygon of \code{x}.  
#'    
#'    Note that \code{halton(1,2,random.start+i)}, for 
#'    \code{i} > 0, is not guaranteed to fall inside a polygon of \code{x}
#'    when scaled by \code{bas.bbox}. The sample consists of the point 
#'    associated with \code{random.start} and the next \code{n-1}
#'    Halton points in sequence that fall inside a polygon
#'    of \code{x}. 
#'    
#'    
#'    \item \code{bas.bbox}: The square bounding box surrounding \code{x}
#'    used to scale Halton points.  A scaled Halton sequence of n points
#'    is \code{bas.bbox[,"min"] +} \code{t(halton(n,2,random.start)) *} 
#'    \code{rep( max(diff(t(bas.bbox))), 2)}.
#'    
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

my.dim <- 2 # number of dimensions we are sampling. leave this here
            # to make it easier to expand to dim > 2

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
# Not sure the following is needed.  I.e., exists() seems to always return 
# TRUE because maxU is in the SDraw namespace.  I don't understand.  
# seems like just "get("maxU")" would get the first instance of maxU,
# whether in .GlobalEnv or SDraw namespace.  Nonetheless, I will leave this 
# code with "exists" in it.
if (exists("maxU", envir = .GlobalEnv, mode="function")) {
  max.u <- get( "maxU", envir=.GlobalEnv, mode="function" )
  max.u <- max.u()
} else {
  max.u <- SDraw::maxU()
}

# Compute bounding box around x. If you want a non-square surrounding x
# set d.box to c(delta.x, delta.y)
d.box <- rep(max(diff(t(bb))), 2)
xl <- bb[1,"min"]
xr <- bb[1,"min"] + d.box[1]
yl <- bb[2,"min"]
yu <- bb[2,"min"] + d.box[2]
bas.bbox <- matrix( c(xl,yl,xr,yu), 2)
dimnames(bas.bbox) <- list(coordnames(x),c("min","max"))

# Find first Halton point after random start that is in polygon
repeat{
  m <- floor( runif( n.init*my.dim, min=0, max=max.u+1 ))
  m <- matrix(m,n.init,my.dim)
  halt.samp <- matrix(NA, n.init, my.dim)

  for(i in 1:n.init){
    halt.samp[i,] <- halton( 1, dim=my.dim, start=m[i,] )
  }
  
  #   Convert from [0,1] to a square box covering [bb]
  halt.samp <- bas.bbox[,"min"] + t(halt.samp) * d.box
  halt.samp <- t(halt.samp)
  
#  cat("dim halt.samp : "); cat(nrow(halt.samp)); cat("\n")
#  cat(paste("n.init=",n.init,"\n"))
                                                    
  halt.pts <- SpatialPointsDataFrame(halt.samp, data=data.frame(sampleID=1:n.init),
                                     proj4string=crs.obj )
  
  in.poly <- over( halt.pts, x )
  
  keep <- !is.na( in.poly$sampleID )
  
  if(any(keep)) break
}



# The following is debugging code.  Uncomment (down to "+=+=+") to see the 
# bounding box around x and the first point in x
# bb.plot <- cbind(c(xl, xr), 
#                  c(yl, yu))
# bb.plot <- SpatialPoints(bb.plot, proj4string = crs.obj)
# plot(bb.plot, pch=16, cex=.1)
# lines(c(xl,xr), rep(yu,2) )
# lines(c(xl,xr), rep(yl,2) )
# lines(rep(xl,2), c(yl,yu) )
# lines(rep(xr,2), c(yl,yu) )
# plot(x, add=T)
# points(halt.pts, pch=16, col="blue")
# points(halt.pts[which(keep)[1]],pch=16,col="red")
# +=+=+

# Keep first that is in a polygon
which.pt.in <- which(keep)[1]  # or min(which(keep))
m <- m[which.pt.in,]
halt.pts <- halt.pts[which.pt.in,]


#   Take initial number of Halton numbers that is approximately correct
#   This is number of samples to take to be Alpha% sure that we get n 
#   points in the study area.  99% of time this loop runs once.
#   At this point, halt.pts has one point in it.
q <- 1 - p
z <- qnorm(0.99)
halt.start <- m  # save for attributes later
n.cur <- n
repeat{
  n.init <- (n.cur / p) + (q*z*z/(2*p)) + (z / p)*sqrt(z*z*q*q/4 + q*n.cur)  # term in sqrt is >0 because we have control on all terms    
  n.init <- ceiling(n.init)
  halt.samp <- halton( n.init, dim=my.dim, start=m+1 )
  
  #   Convert from [0,1] to a square box covering [bb]
  halt.samp <- bas.bbox[,"min"] + t(halt.samp) * d.box
  halt.samp <- t(halt.samp)
  
  #   Check which are in the polygon, after first converting halt.samp to SpatialPoints
  #   And adding to points from previous iteration
  #   sampleID in this data frame gets overwritten below when assign to @data

  crds <- rbind(coordinates(halt.pts), halt.samp)
  halt.pts <- SpatialPointsDataFrame(crds, 
                    data=data.frame(sampleID=1:nrow(crds)),
                    proj4string = crs.obj)

  in.poly <- over( halt.pts, x )
  
  #   Reject the points outside the polygon, and attach other attributes if present
  keep <- !is.na( in.poly$sampleID )  # in.poly$sampleID is row num of polygon in x

  if( sum(keep) >= n ){
      break
  } else {
      n.cur <- n - sum(keep)
      m <- m + n.init  # place in Halton sequence to start next iter minus one (+1 added above in call to Halton)
  }

}  


# Attach attributes
halt.pts@data <- data.frame( in.poly )
halt.pts <- halt.pts[ keep, ]

halt.pts <- halt.pts[1:n,]
halt.pts$sampleID <- 1:n   # renumber the site ID's because some (those outside polygon) were tossed above


attr(halt.pts, "frame") <- deparse(substitute(x))
attr(halt.pts, "frame.type") <- "polygon"
attr(halt.pts, "sample.type") <- "BAS"
attr(halt.pts, "random.start") <- halt.start
attr(halt.pts, "bas.bbox") <- bas.bbox

halt.pts

}
