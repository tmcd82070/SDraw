bas.line <- function(n, shp, n.pixel.factor=5){

#   Function to draw a bas sample from a shapefile
#   containing linear 1-d features.


if( regexpr("Lines", class(shp)) < 0 ) stop("Must call bas.line with a SpatialLinesX object.")

##   Check for projected coordinates and convert if necessary.
#ps <- proj4string(shp)
#if( regexpr("proj=longlat", ps) > 0 ){
#    #   We have a lat-long system  - Issue a warning
#    warning( paste("SDRAW: Lat-Long coordinate system found. Recommend conversion of \n",
#    "original spatial lines to UTM and re-drawing sample. Use of Lat-Long will slightly reduce\n", 
#    "spatial balance when sampling long lines."))
#}

#   Discretize the line with n.pixel.factor more points than needed for sample
pt.frame <- spsample( shp, n*n.pixel.factor, type="regular" )

#   Sample as points
samp <- bas.point( n, pt.frame )

samp

}
