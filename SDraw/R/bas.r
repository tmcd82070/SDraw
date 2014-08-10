bas <- function(n, shp){
#
#   Draw a BAS sample from the shapefile named in fn.
#
#   Inputs:
#   n = desired sample size
#   shp = an sp object (either a SpatialPoints, SpatialLines, or SpatialPolygons)
#
#   Output:
#   An ordered data frame containing n+over.n rows, where each row represents a
#   a different unit selected in the sample.
#



    
    if( length(grep("SpatialPoints", class(shp))) > 0 ){

        samp <- bas.finite( n, shp )
        
    } else if (length(grep("SpatialLines", class(shp))) > 0 ){

        samp <- bas.linear( n, shp )
    
    } else if (length(grep("SpatialPolygons", class(shp))) > 0 ){
    
        samp <- bas.area( n, shp )
    
    } else {
        stop( "Unknown spatial object type" )
    }
    
    samp
}
    

  


