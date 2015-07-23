run.sample <- function(button, dat){


    #   Query the entry fields
    n <- dat$n.entry$getText()
    fn <- dat$shape.in.entry$getText()
    in.dir <- dat$shape.in.dir$getText()
    outobj <- dat$out.r.entry$getText()
    over.n <- dat$over.entry$getText()
    seed <- dat$seed.entry$getText()
    stype <- dat$samp.type.combo$getActiveText()
    stype <- substring(stype, 1, 4)

    #   Set seed if there is a number present
    if( nchar(seed) > 0 ){
        seed <- as.numeric( seed )
        set.seed( seed )
    }

#    #   Get frame type 
#    if( dat$area.rb$getActive() ){
#        sframe.type <- "area"
#    } else if ( dat$line.rb$getActive() ){
#        sframe.type <- "linear"
#    } else {
#        sframe.type <- "finite"
#    }

    
    #   Check input parameters
    if( length(n) == 0 | (n <= 0) ){
        error.message("Sample size not specified or is less than 0.")
        return()
    }

    if( nchar(fn) == 0 | (fn == " ") ){
        error.message("Input shape file not specified.")
        return()
    }

#    if( exists( outobj, where=.GlobalEnv ) ){
#        #   Ideally, we could ask the user here if they want to overwrite.
#        #   This should be easy using RGtk windows, but I am in a rush.
#        #   For now, just save a backup copy
#        assign( paste(outobj, ".previous", sep=""), get(outobj, pos=.GlobalEnv), pos=.GlobalEnv)
#        cat( paste( "Old version of", outobj, "copied to", paste(outobj, ".previous", sep=""), "\n"))
#    }

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

#   Write out the command to console and history file.  Must do this 
#   here, rather than in draw.XXX routines, because I want to write out 
#   the assignment to the output object.
#   NOTE: TIMESTAMP STOPPED WORKING.  AT SOME POINT, RETURN AND WRITE THESE 
#   COMMANDS TO A SDRAW.LOG FILE.
#     cat(">")
#     switch( stype, 
#         "BAS " = timestamp(paste(outobj, "<- bas(", n + over.n, ",", fn, ")"), prefix="", suffix=" ## SDraw"),
#         "GRTS" = timestamp(paste(outobj, "<- grts.equi(", n, ",", over.n, ",", fn, ")"), prefix="", suffix=" ## SDraw"),
#         "SSS " = timestamp(paste(outobj, "<- sss(", n , ",", fn, ")"), prefix="", suffix=" ## SDraw")
#     )


    #   Actually draw the sample
    #   Remember that fn is the text string name of the shapefile with path, but without .shp.
    samp <- switch( stype, 
                "HAL " = draw.hal(n,over.n,fn,in.dir),    
                "BAS " = draw.bas(n,over.n,fn,in.dir),
                "GRTS" = draw.grts(n,over.n,fn,in.dir,outobj),
                "SSS " = draw.sss(n,over.n,fn,in.dir),
                         stop(paste("Unknown sample type:",stype)))

    #   Save the sample in global environment.  Type of sample is an attribute. 
    #  SDrawPackageSpace <- as.environment( "package:SDraw" )
    #  assign( outobj, samp, pos=SDrawPackageSpace )
    assign( outobj, samp, pos=.GlobalEnv )

    #   Tell user we are finished.
    cat("First 10 sample locations:\n")
    print(samp[1:10,])

    dialog <- gtkMessageDialogNew(NULL, c("modal"), "info", "ok", stype, "draw successful.", "\nCode file saved to", outobj, ".")
    dialog$run()
    dialog$destroy()

}
