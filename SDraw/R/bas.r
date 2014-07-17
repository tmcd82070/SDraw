bas <- function(n, sframe.type, fn){
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

    require(spsurvey)

#   Check whether the shapefile has been read already, and the sp object is laying around. 
    existing.fn <- paste( ".", fn, sep="" )
    if( exists(existing.fn) ){
        #   The shapefile has already been read, and it is laying around.  No need to re-read.
        shp <- get(existing.fn)
    } else {
        #   The shapefile is not laying around.  Read it.

        pth.fn <- file.path(.INPUT.DIR, paste(fn,".shp",sep=""))
        if( !file.exists(pth.fn) ){
            error.message(paste("Shapefile", file.path(.INPUT.DIR, fn), "does not exist."))
            return()
        }
        
        start.spinner()
        
        #   Read the shape file
        pth.fn <- file.path(.INPUT.DIR, fn)
        shp <- read.shape( pth.fn )  # Assume sp is attached.  This read can take a while.
        
        assign(existing.fn, shp, pos=.GlobalEnv)  # save a copy for future use
        
        stop.spinner()    
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
    

  


