sss.polygon <- function( n, shp ){

bb <- bbox( shp )

lx <- diff( bb["x",] )
ly <- diff( bb["y",] )

#Abb <- lx*ly   # area of bounding box
A <- sum(unlist(lapply( shp@polygons, function(x){ x@area})))  # area of shapefile

#nstar <- round(n * Abb / A)  # Draw larger sample to account for ones we toss later

#   Now compute spacing.  I've worked this out on paper.
#   Three equations and three unknowns are:
#       delta*nx = lx
#       delta*ny = ly
#       nx*ny = nstar
#delta <- (-(ly+lx) + sqrt( (ly+lx)^2 + 4*nstar*lx*ly )) / (2*nstar)  # THis is wrong, I got one too many rows and columns in it
delta <- sqrt( A / n )


#   The random start
m.x <- runif( 1, 0, delta )
m.y <- runif( 1, 0, delta )


#   The grid
seq.x <- seq( bb["x","min"], bb["x","max"], by=delta ) + m.x
seq.y <- seq( bb["y","min"], bb["y","max"], by=delta ) + m.y


grd <- expand.grid( x=seq.x, y=seq.y )
df <- data.frame( row=rep(1:length(seq.y), each=length(seq.x)), col=rep(1:length(seq.x), length(seq.y)), pointType=rep("Sample", length(seq.x)*length(seq.y)) )
grd <- SpatialPointsDataFrame( grd, data=df )

#   Clip to shp
shp@data <- data.frame( data.frame(shp), zzz=1 )   #  make sure data frame has at least one numeric column
tmp <- over( grd, shp )
keep <- !is.na(tmp$zzz)
tmp <- tmp[,!(names(tmp) %in% c("zzz"))] 
tmp2 <- data.frame(grd)
tmp2 <- tmp2[,!(names(tmp2) %in% c("x","y"))] 
grd@data <- data.frame( tmp2, tmp )
grd <- grd[ keep, ]

#   Add spacing as attribute
attr(grd, "spacing") <- delta

grd

}
