extractPathFilename <- function( in.fn ){

    if(  nchar(in.fn) > 0 ){
        #   Must do this on windows systems, but if we are on a non-Windows system
        #   it should not hurt.
        in.fn <- gsub( "\\", .Platform$file.sep, in.fn, fixed=TRUE )

        #   Now trim off the directory for display in the dialog
        fn.list <-  strsplit( in.fn, .Platform$file.sep, fixed=TRUE )

        #   Get input file
        input.file <- fn.list[[1]][ length(fn.list[[1]]) ]

        #   work out directory and file name
        input.dir <- sub( paste(.Platform$file.sep, input.file, sep=""), "", in.fn )
        input.file <- sub(".shp", "", input.file )
    } else {
        input.dir <- in.fn
        input.file <- in.fn
    }

    list(path=input.dir, file=input.file)
}
