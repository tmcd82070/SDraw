draw.grts <- function(n,over.n,fn){
#
#   draw a GRTS sample using spsurvey.  Spsurvey should already be loaded.
#

#    cat("Drawing GRTS sample...This can take a while ...\n")

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn )

#   Call the user visible routine that takes a SpatialX object
    ans <- grts.equi( n, over.n, shp )

    ans
}
