run.sample <-
function(button, dat){


    n <- .n.entry$getText()
    fn <- .shape.in.entry$getText()
    outobj <- .out.r.entry$getText()
    over.n <- .over.entry$getText()

    #   Set seed if there is a number present
    seed <- .seed.entry$getText()
    if( nchar(seed) > 0 ){
        seed <- as.numeric( seed )
        set.seed( seed )
    }

    #   Get frame type 
    if( .area.rb$getActive() ){
        sframe.type <- "area"
    } else if ( .line.rb$getActive() ){
        sframe.type <- "linear"
    } else {
        sframe.type <- "finite"
    }

    #   Get type of sample
    stype <- gtkComboBoxGetActiveText(.samp.type.combo)
    stype <- substring(stype, 1, 4)
    
    #   Check input parameters
    if( length(n) == 0 | (n <= 0) ){
        error.message("Sample size not specified or is less than 0.")
        return()
    }

    if( nchar(fn) == 0 | (fn == " ") ){
        error.message("Input shape file not specified.")
        return()
    }

    if( exists( outobj, where=.GlobalEnv ) ){
        #   Ideally, we could ask the user here if they want to overwrite.
        #   This should be easy using RGtk windows, but I am in a rush.
        #   For now, just save a backup copy
        assign( paste(outobj, ".previous", sep=""), get(outobj, pos=.GlobalEnv), pos=.GlobalEnv)
        cat( paste( "Old version of", outobj, "copied to", paste(outobj, ".previous", sep=""), "\n"))
    }



    #   fix up the sample sizes
    n <- as.numeric( n )
    if( nchar(over.n) == 0 ){
        over.n <- 0
    } else {
        over.n <- as.numeric( over.n )
    }
    if( is.na(over.n) ){
        warning("Oversample set to 0.")
        over.n <- 0
    }

    #   Actually draw the sample
    #   Remember that fn is the text string name of the shapefile, without .shp, and without path.
    samp <- switch( stype, 
                "BAS " = draw.bas(n,over.n,sframe.type,fn),
                "GRTS" = draw.grts(n,over.n,sframe.type,fn),
                "SSS " = draw.sss(n,over.n,sframe.type,fn),
                         stop(paste("Unknown sample type:",stype)))

    #   Save the sample in global environment.  Type of sample is an attribute.                         
    assign( outobj, samp, pos=.GlobalEnv )

    #   Tell user we are finished.
    cat("First 10 sample locations:\n")
    print(samp[1:10,])

    dialog <- gtkMessageDialogNew(NULL, c("modal"), "info", "ok", stype, "draw successful.")
    dialog$run()
    dialog$destroy()

}
