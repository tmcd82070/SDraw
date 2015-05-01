hal <- function(n, shp, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3)){
#
#   Draw a HAL sample from the shapefile named in fn.
#
#   Inputs:
#   n = desired sample size
#   shp = an sp object (either a SpatialPoints, SpatialLines, or SpatialPolygons)
#   Other inputs: see hal.polygon.r
#   Note: eta and triangular only apply to sampling from a polygon.  I.e., only apply 
#   when shp is a SpatialPolygons* object.
#  
#   Output:
#
####


    
    if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){

        samp <- hal.point( n, shp, J, bases )
        
    } else if (regexpr("SpatialLines", class(shp)[1]) > 0 ){

        #samp <- bas.line( n, shp )
        cat("HAL sample of lines not implemented yet\n")
      
    } else if (regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
    
        samp <- hal.polygon( n, shp, J, eta, triangular, bases )  # this uses defaults for lattice
    
    } else {
        stop( "Unknown spatial object type" )
    }
    
    samp
}
    

  


