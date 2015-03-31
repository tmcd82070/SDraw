grts.strat <- function( n, over.n, strat.var, sizes, shp ){

# these are data passed from the GUI
# strat.name = "Stratum"
# strat.var <- c("Large","Small")
# sizes <- c(120, 30)
# selType=c("Equal")

#this makes a list of elements to be passed to the grts function
Stratdsgn <- lapply(1:length(strat.var), function(x) list(panel=c(PanelOne=sizes[x]),seltype=selType))
names(Stratdsgn) <- strat.var



    # Stratdsgn <- list(None = list(panel = c(sample = n), #update this for stratified call
                                  # seltype = "Equal",
                                  # over = over.n))


    if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){
        sframe.type = "finite"
    } else if( regexpr("SpatialLines", class(shp)[1]) > 0 ){
        sframe.type = "linear"
    } else if( regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
        sframe.type = "area"
    }

    Stratsites <- grts(design=Stratdsgn,
            DesignID="STRAT",
            type.frame=sframe.type,
            src.frame="sp.object",
            sp.object=shp,
			stratum="Stratum",   #need to use stratum variable name as taken from GUI
            shapefile=FALSE)


    cat("Success.\n")

    #   Toss some variables that are not important for equal probability designs
    #Equalsites <- Equalsites[,!(names(Equalsites) %in% c("mdcaty","wgt","stratum","panel"))]

    #   Add a column of sample/oversample for convieneince
   # Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))

    #   Copy over the projection from the input spatial object
    proj4string(Stratsites) <- CRS(proj4string(shp))

    #   Store some attributes
    attr(Stratsites, "sample.type") <- "GRTS"
    #attr(Equalsites, "n") <- n
    #attr(Equalsites, "over.n") <- over.n
    attr(Stratsites, "sp.object") <- deparse(substitute(shp))
    attr(Stratsites, "frame.type") <- sframe.type

    Stratsites
}
