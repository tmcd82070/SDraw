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



    
    if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){

        samp <- bas.point( n, shp )
        
    } else if (regexpr("SpatialLines", class(shp)[1]) > 0 ){

        samp <- bas.line( n, shp )
    
    } else if (regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
    
        samp <- bas.polygon( n, shp )
    
    } else {
        stop( "Unknown spatial object type" )
    }
    
    samp
}
    

  


