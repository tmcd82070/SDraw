browse.for.shapefile <- function(){
#
#

    filt <- rbind( c( "All files (*.*)", "*.*"), c("Shapefiles (*.shp)", "*.shp") )

    #   Prompt the user for file name
    print(.INPUT.DIR)
    in.fn <- choose.files(.INPUT.DIR, multi=FALSE, filters=filt)

    #   Must do this on windows systems
    in.fn <- gsub( "\\", .Platform$file.sep, in.fn, fixed=TRUE )

    #   Now trim off the directory for display in the dialog
    fn.list <-  strsplit( in.fn, .Platform$file.sep, fixed=TRUE )

    #   Store input file
    input.file <- fn.list[[1]][ length(fn.list[[1]]) ]

    #   Reset the input directory

    assign( ".INPUT.DIR", sub( paste(.Platform$file.sep, input.file, sep=""), "", in.fn ), pos=.GlobalEnv )

    input.file <- sub(".shp", "", input.file )

    .shape.in.entry$setText(input.file)


    in.fn
}
