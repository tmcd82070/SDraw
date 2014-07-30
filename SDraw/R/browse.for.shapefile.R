browse.for.shapefile <- function(input.dir,dat){
#
#

    filt <- rbind( c( "All files (*.*)", "*.*"), c("Shapefiles (*.shp)", "*.shp") )

    #   Prompt the user for file name
    #.INPUT.DIR <- get(".INPUT.DIR", .GlobalEnv )
    #print(.INPUT.DIR)
    in.fn <- choose.files(input.dir, multi=FALSE, filters=filt)

    #   Must do this on windows systems, but if we are on a non-Windows system
    #   it should not hurt.
    in.fn <- gsub( "\\", .Platform$file.sep, in.fn, fixed=TRUE )

    #   Now trim off the directory for display in the dialog
    fn.list <-  strsplit( in.fn, .Platform$file.sep, fixed=TRUE )

    #   Get input file
    input.file <- fn.list[[1]][ length(fn.list[[1]]) ]

    #   Return
    input.dir <- sub( paste(.Platform$file.sep, input.file, sep=""), "", in.fn )
    input.file <- sub(".shp", "", input.file )

    dat$shape.in.entry$setText(input.file)

    ans <- list( input.dir=input.dir, input.file=input.file)

}
