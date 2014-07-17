my.write.csv <- function(x){

    samp.nm <- .out.r.entry$getText()

    if( exists( samp.nm ) ){
        samp <- get( samp.nm )
        samp.nm2 <- gsub(".", "_", samp.nm, fixed=TRUE )
        out.fn <- paste( samp.nm2, ".csv", sep="" )
        write.table( samp, file=out.fn, sep=",", col.names=TRUE, row.names=FALSE )
        cat(paste("Sample", samp.nm, "written to", out.fn, "in directory", getwd(), "\n"))
    } else {
        error.message( paste("Sample", samp.nm, "does not exist.") )
    }


}
