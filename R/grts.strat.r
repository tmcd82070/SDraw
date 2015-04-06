grts.strat <- function( n, over.n, strat.var, shp ){

# Inputs: 
  # n = vector of sample sizes, one element per strata
  # over.n = scalar (vector length 1) of number of units to add per strata.  Constant across strata
  # strat.var = string nameing strata variable IF shape contains points or lines
  # shp = the SpatialXDataFrame object (the frame)

# Get strata level names from shape file
strata.levels<-names(table(data.frame(shp[,strat.var])))
  # For debuggin
  cat("---- n -----\n")
  print(n)
  cat("---- over.n -----\n")
  print(over.n)
  cat("---- strat.var -----\n")
  print(strat.var)
  cat("---- strata.levels -----\n")
  print(strata.levels) 
  cat("---- head(shp) -----\n")
  print(head(data.frame(shp)))
  
  #this makes a list of elements to be passed to the grts function
    selType="Equal"
	Stratdsgn <- lapply(1:length(strata.levels), function(x) list(panel=c(PanelOne=n[x]),seltype=selType,over=over.n))
    names(Stratdsgn) <- strata.levels


# strataDsgn <- lapply(1:length(strata.vec), function(x) list(panel=c(PanelOne=strata.sizes.vec[x]),seltype=selType))
# names(strataDsgn) <- strata.vec

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
			stratum=strat.var,   #need to use stratum variable name as taken from GUI
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
    attr(Stratsites, "n") <- n
    attr(Stratsites, "over.n") <- over.n
    attr(Stratsites, "sp.object") <- deparse(substitute(shp))
    attr(Stratsites, "frame.type") <- sframe.type

    Stratsites
}
