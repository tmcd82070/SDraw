plot.sample <- function(button){

    require(sp)
    require(spsurvey)   # just incase we get here via some path other than the GUI. E.g., call directly.
    
    fn <- .shape.in.entry$getText()
    outobj <- .out.r.entry$getText()

    if( nchar(fn) == 0 ){
        error.message("A shapefile name must be specified.")
        return()
    }
    
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
        shp <- read.shape( pth.fn )  # Assume sp is attached.  This takes a while.
        
        assign(existing.fn, shp, pos=.GlobalEnv)
        
        stop.spinner()
    }
    
    #   plot shape file
    if( "SpatialPolygonsDataFrame" %in% class(shp) ){
        plot(shp, col=rainbow(length(shp@polygons)))
    } else {
        plot(shp)
    }

    #   If the sample object exists, plot points on the map
    if( exists( outobj )){
        samp <- get( outobj, pos=.GlobalEnv )
        n <- attr(samp, "n")
        stype <- attr(samp, "sample.type")
        
        if( nrow(samp) == n ){
            #   No oversample
            points( samp, pch=16 )
            legend("bottomleft", legend=paste(stype, "sample points"), pch=c(16))
        } else {
            #   There is some oversample
            points( samp[1:n,], pch=16 )
            points( samp[ (n+1):nrow(samp),], pch=1 )
            legend("bottomleft", legend=paste(stype, c("sample", "over sample")), pch=c(16,1))
        }

    }

}
