bas.point <- function(n, shp){
#   Function to draw a bas sample from a shapefile
#   containing a list of finite features.

if( regexpr("Points", class(shp)) < 0 ) stop("Must call bas.point with a SpatialPointsX object.")

N <- length(shp)

if( n > N){
  warning("Sample size greater than frame requested. Census taken.")
  n <- N
}

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


#   Now that we have polygons around points, call bas.polygon. 
#   But, because points can be sampled more than once when the Halton sequence comes 
#   back around.  To deal with this, take an oversample and keep going until we get n distinct points. 
samp <- NULL
crs.obj <- CRS(shp@proj4string@projargs)
cord.names <- dimnames(bbox(shp))[[1]]
df.col.names <- names(df)


repeat{
  samp2 <- bas.polygon( n * (1 + n/N), pixels )  # at most, a 2*n sample

  #   Snap the halton points to the pixel center points, which were a part of the input data frame
  cords.df <- data.frame(samp2)
  cords <- cords.df[,cord.names]

  samp2 <- SpatialPointsDataFrame( cords, data=cords.df, proj4string=crs.obj )  
  
  if( length(samp) > 0){
    samp.cords <- rbind(coordinates(samp), cords)
    samp.df <- rbind(data.frame(samp)[,df.col.names], cords.df[,df.col.names])
  } else {
    samp.cords <- cords
    samp.df <- cords.df[,df.col.names]
  }
                        
  dups <- duplicated(samp.cords)
  
  samp <- SpatialPointsDataFrame( samp.cords[!dups,], data=samp.df[!dups,], proj4string=crs.obj)
  
  if( length(samp) >= n ){
    samp <- samp[1:n,]
    break
  }  
}


samp

}
