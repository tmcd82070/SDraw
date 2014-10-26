sss.line <- function(n, shp){

if( regexpr("Lines", class(shp)) < 0 ) stop("Must call sss.line with a SpatialLinesX object.")

#   This is easy, use spsample from the sp package.
#   Note: this is the only function that requires the Depends: sp statement in DESCRIPTION.
#   Without this call to spsample, you can Import sp, rather than Depend it.
samp <- spsample( shp, n, type="regular" )

#   Make answer into a SpatialPointsDataFrame to be consistent with other SDraw routines
#   It would be nice to transfer over the attributes of shp, but shp is a line, 
#   and the over() function does not take points over lines (makes sense, no area to lines). 
#   And, I cannot figure out a nice way to buffer the lines and use over.

samp <- SpatialPointsDataFrame( samp,  data=data.frame(siteID=1:length(samp)) )

samp

}
