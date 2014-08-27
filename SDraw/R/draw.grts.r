draw.grts <- function(n,over.n,fn){
#
#   draw a GRTS sample using spsurvey.  Spsurvey should already be loaded.
#

    cat("Drawing GRTS sample...This can take a while ...\n")
    
    Equaldsgn <- list(None = list(panel = c(sample = n),
                                  seltype = "Equal",
                                  over = over.n))

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn )

    if( attr(shp, "type") == "points" ){
        sframe.type = "finite"
    } else if( attr(shp, "type") == "lines" ){
        sframe.type = "linear"
    } else if( attr(shp, "type") == "polygons" ){
        sframe.type = "area"
    }
    
    Equalsites <- grts(design=Equaldsgn,
            DesignID="EQUAL",
            type.frame=sframe.type,
            src.frame="sp.object",
            sp.object=shp,
            shapefile=FALSE)



    cat("Success.\n")

    #   Toss some variables that are not important for equal probability designs
    Equalsites <- Equalsites[,!(names(Equalsites) %in% c("mdcaty","wgt","stratum","panel"))]
    
    #   Add a column of sample/oversample for convieneince
    Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))

    #   Store some attributes
    attr(Equalsites, "sample.type") <- "GRTS"
    attr(Equalsites, "n") <- n
    attr(Equalsites, "over.n") <- over.n
    attr(Equalsites, "sp.object") <- fn
    attr(Equalsites, "frame.type") <- sframe.type

    Equalsites
}
