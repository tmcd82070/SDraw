grts.equi <- function( n, over.n, shp ){

    Equaldsgn <- list(None = list(panel = c(Main = n),
                                  seltype = "Equal",
                                  over = over.n))


    if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){
        sframe.type = "finite"
    } else if( regexpr("SpatialLines", class(shp)[1]) > 0 ){
        sframe.type = "linear"
    } else if( regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
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
    #Equalsites <- Equalsites[,!(names(Equalsites) %in% c("mdcaty","wgt","stratum","panel"))]

    #   Add a column of sample/oversample for convieneince
    Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))

    #   Copy over the projection from the input spatial object
    proj4string(Equalsites) <- CRS(proj4string(shp))

    #   Store some attributes
    attr(Equalsites, "sample.type") <- "GRTS"
    attr(Equalsites, "n") <- n
    attr(Equalsites, "over.n") <- over.n
    attr(Equalsites, "sp.object") <- deparse(substitute(shp))
    attr(Equalsites, "frame.type") <- sframe.type

    Equalsites
}
