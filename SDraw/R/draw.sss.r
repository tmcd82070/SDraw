draw.sss <- function(n,over.n,fn){
#
#   draw a sss sample.
#

    

    if( over.n > 0 ) warning( "Oversampling for systematic samples not allowed. Oversample set to zero." )
    

#   Check whether the shapefile has been read already, and the sp object is laying around. 
    existing.fn <- fn
    if( exists(existing.fn) ){
        #   The shapefile has already been read, and it is laying around.  No need to re-read.
        shp <- get(existing.fn)
    } else {
        #   The shapefile is not laying around.  Read it.
        input.dir <- get(".INPUT.DIR")

        shp <- readShape(input.dir, fn)  # a wrapper for readOGR
        
        assign(existing.fn, shp, pos=.GlobalEnv)  # save a copy for future use
    }

#   Determin the sample type and call the appropriate function
    if( length(grep("SpatialPoints", class(shp))) > 0 ){

        stop( "SSS samples of points not yet implemented in SDraw")
        #samp <- sss.finite( n, shp )
        
    } else if (length(grep("SpatialLines", class(shp))) > 0 ){

        stop( "SSS samples of lines not yet implemented in SDraw")
        #samp <- sss.linear( n, shp )
    
    } else if (length(grep("SpatialPolygons", class(shp))) > 0 ){
    
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
