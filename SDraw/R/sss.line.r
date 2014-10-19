sss.line <- function(n, shp){

if( regexpr("Lines", class(shp)) < 0 ) stop("Must call sss.line with a SpatialLinesX object.")

#   This is easy, use spsample from the sp package.
#   Note: this is the only function that requires the Depends: sp statement in DESCRIPTION.
#   Without this call to spsample, you can Import sp, rather than Depend it.
samp <- spsample( shp, n, type="regular" )


samp

}
