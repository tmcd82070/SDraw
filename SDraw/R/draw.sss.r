draw.sss <- function(n,over.n,fn){
#
#   draw a sss sample.
#

    

    if( over.n > 0 ) warning( "Oversampling for systematic samples not allowed. Oversample set to zero." )
    

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn )


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
