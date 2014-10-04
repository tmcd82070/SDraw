bas.point <- function(n, shp){
#   Function to draw a bas sample from a shapefile
#   containing a list of finite features.

if( regexpr("Points", class(shp)) < 0 ) stop("Must call bas.point with a SpatialPointsX object.")

pts <- coordinates( shp )

#   Find minimum distance between two points.  
#   This is expensive, but necessary. 
d <- min(stats::dist( pts ))  # minimum distance.  Use dist from stats package. Keep in mind this is decimal degrees if shp is lat long.

#   Make pixels around points 
d <- d / (2*sqrt(2))   # reduce minimum so no pixels over lap
ll.x <- pts[,1] - d
ll.y <- pts[,2] - d
ur.x <- pts[,1] + d
ur.y <- pts[,2] + d

pixels <- vector( "list", nrow(pts) )
for( i in 1:nrow(pts)){
    Sr1 <- Polygon( cbind( c(ll.x[i],ll.x[i],ur.x[i],ur.x[i],ll.x[i]), c(ll.y[i],ur.y[i],ur.y[i],ll.y[i],ll.y[i]) ) )
    pixels[[i]] <- Polygons( list( Sr1 ), paste("p",i,sep="") )    
}
pixels <- SpatialPolygons( pixels, 1:nrow(pts), proj4string=CRS(proj4string(shp))   )

#   Now that we have polygons around points, call bas.polygon
samp <- bas.polygon( n, pixels )

#   Snap the halton points to the pixel center points
in.poly <- over( pixels, samp )
print(sum(!is.na(in.poly)))

samp <- pixels[ which(!is.na(in.poly)), ]

samp

}
