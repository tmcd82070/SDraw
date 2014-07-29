draw.sss <- function(n,over.n,sframe.type,fn,input.dir){
#
#   draw a sss sample.
#

    

    if( over.n > 0 ) warning( "Oversampling for systematic samples not allowed. Oversample set to zero." )
    

#   Check whether the shapefile has been read already, and the sp object is laying around. 
    existing.fn <- paste( ".", fn, sep="" )
    if( exists(existing.fn) ){
        #   The shapefile has already been read, and it is laying around.  No need to re-read.
        shp <- get(existing.fn)
    } else {
        #   The shapefile is not laying around.  Read it.

        pth.fn <- file.path(input.dir, paste(fn,".shp",sep=""))
        if( !file.exists(pth.fn) ){
            error.message(paste("Shapefile", file.path(input.dir, fn), "does not exist."))
            return()
        }
        
        startSpinner()
        
        #   Read the shape file
        pth.fn <- file.path(input.dir, fn)
        shp <- read.shape( pth.fn )  # Assume spsurvey is attached.  This read can take a while.
        
        assign(existing.fn, shp, pos=.GlobalEnv)  # save a copy for future use
        
        stopSpinner()    
    }

#   Determin the sample type and call the appropriate function
    if( "SpatialPointsDataFrame" %in% class(shp) ){
        if( sframe.type != "finite" ){
            stop( paste( sframe.type, "sample type specified, but POINTS were found in", fn, "\n"))
        }

        stop( "SSS samples of points not yet implemented in SDraw")
        #samp <- sss.finite( n, shp )
        
    } else if ("SpatialLinesDataFrame" %in% class(shp) ){
        if( sframe.type != "linear" ){
            stop( paste( sframe.type, "sample type specified, but LINES were found in", fn, "\n"))
        }

        stop( "SSS samples of lines not yet implemented in SDraw")
        #samp <- sss.linear( n, shp )
    
    } else if ("SpatialPolygonsDataFrame" %in% class(shp) ){
        if( sframe.type != "area" ){
            stop( paste( sframe.type, "sample type specified, but POLYGONS were found in", fn, "\n"))
        }
    
        samp <- sss.polygon( n, shp )
    
    } else {
        stop( "Unknown spatial object type" )
    }
    

    
    #   Add a column of sample/oversample for convieneince
    #samp$pointType <- rep("Sample",n)

    #   Store some attributes
    attr(samp, "sample.type") <- "SSS"
    attr(samp, "n") <- length(samp)
    attr(samp, "over.n") <- 0
    attr(samp, "frame.type") <- sframe.type
    attr(samp, "shapefile") <- fn

    cat(paste("Realized SSS size =", length(samp), "\n"))
    cat(paste("Realized SSS spacing =", attr(samp,"spacing"), "\n"))

    samp
}
