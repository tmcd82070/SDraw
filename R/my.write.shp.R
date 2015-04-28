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
        dialog <- gtkFileChooserDialog("Export Sample As...", dat$parent.window, "save",
                                       "gtk-save", GtkResponseType["accept"], 
                                       "gtk-cancel", GtkResponseType["cancel"])
    
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
            path.n.filename <- dialog$getFilename()
            out.format <- export.format.combo$getActiveText()
            dialog$destroy()

            if( length(grep("SHP", out.format)) > 0 ){
                path.n.filename <- extractPathFilename(path.n.filename)
                writeOGR( samp, path.n.filename$path, path.n.filename$file, "ESRI Shapefile", morphToESRI=TRUE )
            } else if( length(grep("CSV", out.format)) > 0 ){
                samp$coord.x <- coordinates(samp)[,1]
                samp$coord.y <- coordinates(samp)[,2]
                writeOGR( samp, path.n.filename, samp.nm, "CSV" )
            } else if( length(grep("KML", out.format)) > 0 ){
                pjs <- proj4string(samp)
                if( !is.na(pjs) ){
                    #   There is a projection defined.  Try to convert to lat-long, WGS84 as required by Google Earth
                    if( (length(grep("proj=longlat",pjs,fixed=T))==0) | (length(grep("datum=WGS84",pjs,fixed=T))==0) ){
                        samp <- spTransform( samp, CRS( "+init=epsg:4326" ))
                    }
                }
                #   Regardless, write out the KML.  A warning is issued if projection is anything other 
                #   than "geographical" = lat-long
                writeOGR( samp, path.n.filename, samp.nm, "KML" )
            } else if( length(grep("GPX", out.format)) > 0 ){
                pjs <- proj4string(samp)
                if( !is.na(pjs) ){
                    #   There is a projection defined.  Try to convert to lat-long, WGS84 as required by Google Earth
                    if( (length(grep("proj=longlat",pjs,fixed=T))==0) | (length(grep("datum=WGS84",pjs,fixed=T))==0) ){
                        samp <- spTransform( samp, CRS( "+init=epsg:4326" ))
                    }
                }            
                writeOGR( samp, path.n.filename, samp.nm, "GPX", dataset_options="GPX_USE_EXTENSIONS=YES" )
            } 
          
        } else {
            dialog$destroy()
        }


    } else {
        cat( paste( "Sample object '", samp.nm, "' not found. Hit RUN before EXPORT.\n", sep=""))
    }

}
