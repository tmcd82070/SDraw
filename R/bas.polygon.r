bas.polygon <- function( n, shp ){
#
#   Take a BAS sample from a polygon.
#
#   input:
#   n = desired sample size,
#   shp = a SpatialPolygonsDataFrame object, according to package sp.
#
#   output a data frame containing the BAS sample.

#   Check n
if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
}

#   Find bounding box around everything
bb <- bbox( shp )

#   Find area of all polygons
area <- gArea(shp)  # If shp is not projected, this generates a warning. 

#   Find fraction of the square Halton box covered by the polygons
p <- min(1, area / max(diff(t(bb)))^2 )

#   Maximum number for random start.  Random start is uniform on integers between 1 and this number. 
max.u <- 10e7


my.dim <- 2 # number of dimensions we are sampling

m <- runif( 200 )   # burn 200 random numbers.   I have doubts about the randomness of the first few numbers of R's runif



#   Make sure there is a non-missing attribute associated with each polygon in shp.
#   This is because over() extracts attributes of shp, and missingness is used 
#   to flag which points are outside a polygon.
if( regexpr( "DataFrame", class(shp)) > 0 ){
  #   Shp has a data frame
  df <- data.frame( data.frame(shp), sp.object.ID=row.names(shp), polygon.num=1:length(shp) )
} else {
  df <- data.frame( sp.object.ID=row.names(shp), polygon.num=1:length(shp), row.names=row.names(shp) )
}

shp <- SpatialPolygonsDataFrame( shp, data=df )

crs.obj <- CRS(shp@proj4string@projargs)

#   Draw initial random start, but make sure the first point is inside the study area.
repeat{
  m <- ceiling(max.u * runif( my.dim ))
  halt.samp <- matrix( halton( 1, my.dim, m), 1, 2)

  #   Convert from [0,1] to a square box covering [bb]
  halt.samp <- bb[,"min"] + t(halt.samp) * rep( max(diff(t(bb))), 2)
  halt.samp <- t(halt.samp)
  
  halt.pts <- SpatialPointsDataFrame(halt.samp, proj4string=crs.obj, data=data.frame(siteID=1) )
  
  in.poly <- over( halt.pts, shp )
  keep <- any(!is.na( in.poly$polygon.num ))

  if(keep) break
}

#   Take initial number of Halton numbers that is approximately correct
#   This is number of samples to take to be Alpha% sure that we get n points in the study area.
q <- 1 - p
z <- qnorm(0.99)
n.init <- (n / p) + (q*z*z/(2*p)) + (z / p)*sqrt(z*z*q*q/4 + q*n)  # term in sqrt is >0 because we have control on all terms
n.init <- ceiling(n.init)
halt.samp <- halton( n.init, my.dim, m )

#   Convert from [0,1] to a square box covering [bb]
halt.samp <- bb[,"min"] + t(halt.samp) * rep( max(diff(t(bb))), 2)
halt.samp <- t(halt.samp)


#   Check which are in the polygon, after first converting halt.samp to SpatialPoints
halt.pts <- SpatialPointsDataFrame(halt.samp, proj4string=crs.obj, data=data.frame(siteID=1:nrow(halt.samp)) )

in.poly <- over( halt.pts, shp )


#   Reject the points outside the polygon, and attach other attributes if present
keep <- !is.na( in.poly$polygon.num )
halt.pts@data <- data.frame( in.poly )
halt.pts <- halt.pts[ keep, ]



#   The way we computed n.init, there should be more points in halt.pts than we need. Keep the initial ones.
if( nrow(halt.pts) >= n ){
    halt.pts <- halt.pts[1:n,]
    halt.pts$siteID <- 1:n   # renumber the site ID's because some (those outside polygon) were tossed above
} else {
    warning(paste("Fewer than", n, "points realized. Run again and append or increase sample size."))
}

attr(halt.pts, "halton.seed") <- m

halt.pts

}
