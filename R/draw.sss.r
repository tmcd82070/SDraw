draw.sss <- function(n,over.n,fn,dir){
#
#   draw a sss sample.
#
#   Trent's comment
    

    if( over.n > 0 ) warning( "Oversampling for systematic samples not allowed. Oversample set to zero." )
    

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn, dir )


#   Draw the sample
    samp <- sss( n, shp )

    #   Store some attributes
    attr(samp, "sample.type") <- "SSS"
    attr(samp, "n") <- length(samp)
    attr(samp, "over.n") <- 0
    attr(samp, "frame.type") <- attr(shp, "type")
    attr(samp, "shapefile") <- fn

    cat(paste("Realized SSS size =", length(samp), "\n"))
    cat(paste("Realized SSS spacing =", attr(samp,"spacing"), "\n"))

    samp
}
