draw.bas <- function(n,over.n,sframe.type,fn,input.dir){
#
#   draw a BAS sample.
#


    Equalsites <- bas(n+over.n,sframe.type,fn,input.dir)

    
    #   Add a column of sample/oversample for convieneince
    Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))



    #   Store some attributes
    attr(Equalsites, "sample.type") <- "BAS"
    attr(Equalsites, "n") <- n
    attr(Equalsites, "over.n") <- over.n
    attr(Equalsites, "frame.type") <- sframe.type
    attr(Equalsites, "shapefile") <- fn


    Equalsites
}
