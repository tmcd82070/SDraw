#' @export bas.point
#' 
#' @title Draws a Balanced Acceptance Sample (BAS) from a discrete resource (points).
#' 
#' @description Draws a BAS sample from a \code{SpatialPoints*} object.
#' 
#' @details The BAS method for points computes the minimum distance between 
#' any two points in
#' \code{x} and places a small square (pixel) around each.  Size of the
#' square around each point is d/sqrt(2) on a side, where d is the minimum
#' distance between points. The BAS method for points then selects a BAS sample
#' from the set of polygons (i.e., squares) surrounding each point (see
#' \code{\link{bas.polygon}}).  The BAS method of polygons selects Halton
#' points until \code{n} points are located inside the squares surrounding the
#' points.  When a square contains a Halton point, the official sample location
#' is the the original point (center of the square), not the Halton point. 
#' 
#' @param x A SpatialPoints or SpatialPointsDataFrame object. This object
#' must contain at least 1 point.
#' @param n Sample size.  Number of points to select from the set of points
#' contained in \code{x}.
#'
#' @return A \code{SpatialPointsDataFrame} containing locations in the BAS sample, 
#' in BAS order.
#'  Attributes of the sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  This 
#'   encodes the BAS order.   If BAS order is lost, \code{return[} \code{order(} 
#'   \code{return$sampleID} \code{),]} will resort the 
#'   returned object (i.e., \code{return}) into BAS order.
#'   \item \code{geometryID}: The ID of the point in \code{x} that has been
#'   selected. The 
#'   ID of points in \code{x} are \code{row.names(x)}. 
#'   \item Any attributes of the original lines (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "point").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "BAS").
#'    \item \code{random.start}: The random seed of the random-start Halton sequence 
#'    that produced the sample.  This is a vector of length 2 whose elements are 
#'    random integers between 0 and \code{\link{maxU}}. 
#'    This routine ensures that the point
#'    associated with this index  
#'    falls inside a polygon of interest.  i.e., 
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
#'    
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{bas.polygon}}, \code{\link{bas.line}}, \code{\link{spsample}}
#' @keywords design survey
#' @examples
#' 
#' \dontrun{
#' bas.point( WA.cities, 100)
#' }
#' 
#' 
#' 
bas.point <- function(x, n){
#   Function to draw a bas sample from a shapefile
#   containing a list of finite features.

  if( !(inherits(x, "SpatialPoints"))  ) stop("Must call bas.point with a SpatialPoints* object.")

  N <- length(x)

  if( n > N){
    warning("Sample size greater than frame size. Census taken.")
    n <- N
  }

  pts <- coordinates( x )
  rnames.x <- row.names( x )
  
  #   Find minimum distance between two points.  
  #   This is expensive, but necessary. 
  #   Use dist from stats package. Keep in mind this is decimal degrees if x is lat long.
  d <- min(stats::dist( pts ))  # minimum distance.  
  
  # error trap: check for duplicate coordinates 
  # if duplicates exist: pixels won't draw correctly
  if(d < (.Machine$double.eps * 10)) {
    
    stop("Minimum distance between points is zero: are there duplicate coordinates?")
  
  }

  #   Make pixels around points 
  d <- d / (2*sqrt(2))   # reduce minimum so no pixels over lap
  ll.x <- pts[,1] - d
  ll.y <- pts[,2] - d
  ur.x <- pts[,1] + d
  ur.y <- pts[,2] + d

  #   Make SpatialPolygons object
  pixels <- vector( "list", nrow(pts) )
  for( i in 1:nrow(pts)){
      Sr1 <- Polygon( cbind( c(ll.x[i],ll.x[i],ur.x[i],ur.x[i],ll.x[i]), c(ll.y[i],ur.y[i],ur.y[i],ll.y[i],ll.y[i]) ) )
      pixels[[i]] <- Polygons( list( Sr1 ), rnames.x[i] )    
  }
  pixels <- SpatialPolygons( pixels, 1:nrow(pts), proj4string=CRS(proj4string(x))   )
  
  # Create data frame for pixels, need coordinates of original points 
  # for snapping later.
  if( inherits( x, "SpatialPointsDataFrame" )){
      df <- data.frame( data.frame(x), row.names=row.names(pixels))  # coordinates are included here, by default
  } else {
      df <- data.frame( pts, row.names=row.names(pixels))
  }

  pixels <- SpatialPolygonsDataFrame( pixels, data=df )

  #   Now that we have polygons around points, call bas.polygon. 
  #   But, points can be sampled more than once when the Halton sequence comes 
  #   back around.  To deal with this, take an oversample and keep going 
  #   until we get n distinct points. 
  samp <- NULL
  crs.obj <- CRS(x@proj4string@projargs)
  cord.names <- dimnames(bbox(x))[[1]]
  df.col.names <- c("sampleID", "geometryID", names(df))  # cols to keep in output data frame
  df.col.names <- df.col.names[!(df.col.names %in% c(cord.names,"optional"))]

  n.iter <- n
  repeat{
    # Geometry ID returned by bas.polygon is correct because we assigned row.names when we made pixels
    samp2 <- bas.polygon( pixels, round(n.iter * (1 + n.iter/N)) )  # at most, a 2*n sample
    bas.bbox <- attr(samp2, "bas.bbox")
    ran.start <- attr(samp2,"random.start")  # assign later to m if first time through


    #   Snap the halton points to the pixel center points, 
    #   which were a part of the input data frame
    cords.df <- samp2@data
    cords <- cords.df[,cord.names]
  
    samp2 <- SpatialPointsDataFrame( cords, data=cords.df, proj4string=crs.obj )  
    
    if( length(samp) > 0){
      samp.cords <- rbind(coordinates(samp), cords)
      samp.df <- rbind(data.frame(samp)[,df.col.names,drop=FALSE], cords.df[,df.col.names,drop=FALSE])
    } else {
      samp.cords <- cords
      samp.df <- cords.df[,df.col.names,drop=FALSE]
      m <- ran.start  # save for later
    }
                          
    dups <- duplicated(samp.df$geometryID)
    

    samp <- SpatialPointsDataFrame( samp.cords[!dups,], data=samp.df[!dups,], 
                                    proj4string=crs.obj)


    if( length(samp) >= n ){
      samp <- samp[1:n,]
      break
    }  
    
    # Prep for next iteration, cut down sample size and frame.
    n.iter <- n - length(samp)
    pixels <- pixels[ !(row.names(pixels) %in% samp$geometryID), ]
  }

  samp@data$sampleID <- 1:nrow(samp)
  
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "point"
  attr(samp, "sample.type") <- "BAS"
  attr(samp, "random.start") <- m
  attr(samp, "bas.bbox") <- bas.bbox
  
  samp

}
