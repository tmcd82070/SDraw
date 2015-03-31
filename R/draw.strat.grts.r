draw.strat.grts <- function(over.n,strat.var,sizes,fn,dir){   #took out n, don't think we need this at this point, GTD
#
#   draw a GRTS sample using spsurvey.  Spsurvey should already be loaded.
#

#    cat("Drawing GRTS sample...This can take a while ...\n")

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn, dir )


#   Call the user visible routine that takes a SpatialX object
    ans <- grts.strat( over.n, strat.var, sizes, shp ) #took out n, don't think we need this at this point, GTD

    ans
}
