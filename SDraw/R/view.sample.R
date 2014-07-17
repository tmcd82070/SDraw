view.sample <- function(x){

    samp.nm <- .out.r.entry$getText()

    if( exists( samp.nm, envir=.GlobalEnv ) ){
        samp <- get( samp.nm, pos=.GlobalEnv )
        View( samp, samp.nm )
    } else {
        error.message(paste( "Sample object", samp.nm, "does not exist."))
    }

}
