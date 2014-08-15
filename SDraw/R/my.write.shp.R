my.write.shp <- function(x,dat){
#
#   for next version, call this "Export Sample" 
#   and open a dialog and ask for the format (from amoung those 
#   supported by writeOGR - see ogrDrivers()$name

    samp.nm <- dat$out.r.entry$getText()

    if( exists( samp.nm ) ){
        samp <- get( samp.nm )
        samp.nm2 <- gsub(".", "_", samp.nm, fixed=TRUE )

        # This is how one creates a dialog with buttons and associated response codes.
        dialog <- gtkFileChooserDialog("Export Sample As...", win, "save",
                                       "gtk-cancel", GtkResponseType["cancel"], 
                                       "gtk-save", GtkResponseType["accept"])
    
        #   Define the "extra Widget" for format, and add it to the FileChooseDialog    
        export.formats <- c("ESRI Shapefile (.SHP)", "Comma Separated (.CSV)", "Google Earth (.KML)", "Garmin format(.GPX)" )
        export.format.combo <- gtkComboBoxNewText()
        export.format.combo$show()
        for( i in export.formats ){
            export.format.combo$appendText( i )
        }
        export.format.combo$setActive(0)
    
        combo.box <- gtkHBoxNew(FALSE, 8)
        combo.box$setBorderWidth(8)
        combo.box$add( export.format.combo ) # put the combo box in a HBox to get the spacing
    
        export.format.frame <- gtkFrameNew("Export format:")
        export.format.frame$setBorderWidth(8)
        export.format.frame$add( combo.box )
    
        dialog$setExtraWidget( export.format.frame )
    
        #   run dialog    
        if (dialog$run() == GtkResponseType["accept"]) {
          #   They pressed "Save"
          path.n.filename <- extractPathFilename(dialog$getFilename())
          filename <- path.n.filename$file
          path <- path.n.filename$path
          out.format <- export.format.combo$getActiveText()

            if( length(grep("SHP", out.format)) > 0 ){
                writeOGR( samp, path, filename, "ESRI Shapefile", morphToESRI=TRUE )
            } else if( length(grep("CSV", out.format)) > 0 ){
                writeOGR( samp, path, filename, "CSV" )
            } else if( length(grep("KML", out.format)) > 0 ){
                writeOGR( samp, path, filename, "KML" )
            } else if( length(grep("GPX", out.format)) > 0 ){
                writeOGR( samp, path, filename, "GPX" )
            } 
            cat(paste("Sample", samp.nm, "written to file", filename, "\n in directory", path,"\n in", out.format,"format.\n"))
          
        }
        
        dialog$destroy()


    } else {
        cat( paste( "Sample object '", samp.nm, "' not found. Hit RUN before EXPORT.\n", sep=""))
    }

}
