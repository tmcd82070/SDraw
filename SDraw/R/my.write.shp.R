my.write.shp <- function(x,dat){
#
#   for next version, call this "Export Sample" 
#   and open a dialog and ask for the format (from amoung those 
#   supported by writeOGR - see ogrDrivers()$name

    samp.nm <- dat$out.r.entry$getText()

    if( exists( samp.nm ) ){
        samp <- get( samp.nm )
        samp.nm2 <- gsub(".", "_", samp.nm, fixed=TRUE )
        writeOGR( samp, getwd(), samp.nm2, "ESRI Shapefile", morphToESRI=TRUE )
        cat(paste("Sample", samp.nm, "written to shapefile", samp.nm, "in directory", getwd(),"\n"))
    } else {
        cat( paste( "Sample", samp.nm, "does not exist.\n"))
    }

}
