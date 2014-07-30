bas <- function(n, sframe.type, fn, input.dir){
#
#   Draw a BAS sample from the shapefile named in fn.
#
#   Inputs:
#   n = desired sample size
#   over.n = desired over sample size
#   sframe.type = the type of units in the frame. Values are "area" for
#       polygons, "linear" for line resources, and "finite" for discrete
#       sample units.
#
#   Output:
#   An ordered data frame containing n+over.n rows, where each row represents a
#   a different unit selected in the sample.
#


#   Check whether the shapefile has been read already, and the sp object is laying around. 
    existing.fn <- paste( ".", fn, sep="" )
    if( exists(existing.fn) ){
        #   The shapefile has already been read, and it is laying around.  No need to re-read.
        shp <- get(existing.fn)
    } else {
        #   The shapefile is not laying around.  Read it.
        shp <- readShape(input.dir, fn)  # a wrapper for readOGR

        assign(existing.fn, shp, pos=.GlobalEnv)  # save a copy for future use
    }

    
    if( "SpatialPointsDataFrame" %in% class(shp) ){
        if( sframe.type != "finite" ){
            stop( paste( sframe.type, "sample type specified, but POINTS were found in", fn, "\n"))
        }

        samp <- bas.finite( n, shp )
        
    } else if ("SpatialLinesDataFrame" %in% class(shp) ){
        if( sframe.type != "linear" ){
            stop( paste( sframe.type, "sample type specified, but LINES were found in", fn, "\n"))
        }

        samp <- bas.linear( n, shp )
    
    } else if ("SpatialPolygonsDataFrame" %in% class(shp) ){
        if( sframe.type != "area" ){
            stop( paste( sframe.type, "sample type specified, but POLYGONS were found in", fn, "\n"))
        }
    
        samp <- bas.polygon( n, shp )
    
    } else {
        stop( "Unknown spatial object type" )
    }
    
    samp
}
    

  


