my.write.shp <- function(x){

    samp.nm <- .out.r.entry$getText()

    if( exists( samp.nm ) ){
        samp <- get( samp.nm )
        samp.nm2 <- gsub(".", "_", samp.nm, fixed=TRUE )
        sp2shape( samp, shpfilename=samp.nm2 )
        cat(paste("Sample", samp.nm, "written to shapefile", samp.nm, "in directory", getwd(),"\n"))
    } else {
        cat( paste( "Sample", samp.nm, "does not exist.\n"))
    }

}
