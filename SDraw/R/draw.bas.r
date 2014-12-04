draw.bas <- function(n,over.n,fn,dir){
#
#   draw a BAS sample.
#

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn, dir )

#   Draw the BAS sample
    Equalsites <- bas(n+over.n,shp)

    
    #   Add a column of sample/oversample for convieneince
    Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))



    #   Store some attributes
    attr(Equalsites, "sample.type") <- "BAS"
    attr(Equalsites, "n") <- n
    attr(Equalsites, "over.n") <- over.n
    attr(Equalsites, "frame.type") <- attr(shp,"type")
    attr(Equalsites, "sp.object") <- fn


    Equalsites
}
