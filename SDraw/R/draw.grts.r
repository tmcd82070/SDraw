draw.grts <- function(n,over.n,fn){
#
#   draw a GRTS sample using spsurvey.  Spsurvey should already be loaded.
#

    cat("Drawing GRTS sample...This can take a while ...\n")
    
    Equaldsgn <- list(None = list(panel = c(sample = n),
                                  seltype = "Equal",
                                  over = over.n))

#    pth.fn <- file.path(input.dir, fn)

    sp.obj <- get(fn, .GlobalEnv)
    if( attr(sp.obj, "type") == "points" ){
        sframe.type = "finite"
    } else if( attr(sp.obj, "type") == "lines" ){
        sframe.type = "linear"
    } else if( attr(sp.obj, "type") == "polygons" ){
        sframe.type = "area"
    }
    
    Equalsites <- grts(design=Equaldsgn,
            DesignID="EQUAL",
            type.frame=sframe.type,
            src.frame="sp.object",
            sp.object=fn,
            shapefile=FALSE)

#                in.shape=pth.fn,


    cat("Success.\n")

    #   Toss some variables that are not important for equal probability designs
    Equalsites <- Equalsites[,!(names(Equalsites) %in% c("mdcaty","wgt","stratum","panel"))]
    
    #   Add a column of sample/oversample for convieneince
    Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))

    #   Store some attributes
    attr(Equalsites, "sample.type") <- "GRTS"
    attr(Equalsites, "n") <- n
    attr(Equalsites, "over.n") <- over.n
    attr(Equalsites, "frame.type") <- sframe.type
    attr(Equalsites, "sp.object") <- fn

    Equalsites
}
