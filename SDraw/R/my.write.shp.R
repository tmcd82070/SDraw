my.write.shp <- function(x,dat){
#
#   for next version, call this "Export Sample" 
#   and open a dialog and ask for the format (from amoung those 
#   supported by writeOGR - see ogrDrivers()$name

    samp.nm <- dat$out.r.entry$getText()

    if( exists( samp.nm ) ){
        samp <- get( samp.nm )
        samp.nm2 <- gsub(".", "_", samp.nm, fixed=TRUE )

        exportDialog()
        
        ex.info <- get( ".LAST.EXPORT" )
        
        if( !is.na( ex.info$ex.format ) ){
            #   They pressed "ok"
            if( length(grep("SHP", ex.info$ex.format)) > 0 ){
                writeOGR( samp, ex.info$ex.dir, ex.info$ex.file, "ESRI Shapefile", morphToESRI=TRUE )
            } else if( length(grep("CSV", ex.info$ex.format)) > 0 ){
                writeOGR( samp, ex.info$ex.dir, ex.info$ex.file, "CSV" )
            } else if( length(grep("KML", ex.info$ex.format)) > 0 ){
                writeOGR( samp, ex.info$ex.dir, ex.info$ex.file, "KML" )
            } else if( length(grep("GPX", ex.info$ex.format)) > 0 ){
                writeOGR( samp, ex.info$ex.dir, ex.info$ex.file, "GPX" )
            } 
            cat(paste("Sample", samp.nm, "written to file", ex.info$ex.file, "in directory", ex.info$ex.dir,"in",ex.info$ex.format,"format.\n"))
        }
    } else {
        cat( paste( "Sample", samp.nm, "does not exist. Hit 'Run' before 'Export'.\n"))
    }

}
