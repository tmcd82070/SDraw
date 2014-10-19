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

if( regexpr( "DataFrame", class(shp) ) > 0){
    df <- data.frame(shp, row.names=row.names(pixels))  # coordinates are included here, by default
} else {
    df <- data.frame(pts, row.names=row.names(pixels))
}

pixels <- SpatialPolygonsDataFrame( pixels, data=df )

#   Now that we have polygons around points, call bas.polygon
samp <- bas.polygon( n, pixels )


#   Snap the halton points to the pixel center points, which were a part of the input data frame
cord.names <- dimnames(bbox(shp))[[1]]
cords.df <- data.frame(samp)
cords <- cords.df[,cord.names]
cords.df <- cords.df[,!(names(cords.df) %in% c(cord.names,paste(cord.names,".1",sep="")))]  # Drop coordinates from attributes
pts <- SpatialPointsDataFrame( cords, data=cords.df )



pts
}
