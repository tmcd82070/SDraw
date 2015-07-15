grts.strat <- function( n, over.n, strat.var, shp, fn, dir, outobj ){

# Inputs: 
  # n = vector of sample sizes, one element per strata
  # over.n = scalar (vector length 1) of number of units to add per strata.  Constant across strata
  # strat.var = string nameing strata variable IF shape contains points or lines
  # shp = the SpatialXDataFrame object (the frame)

  # Get strata level names from shape file
  strata.levels<-names(table(data.frame(shp)[,strat.var]))
  
  # For debuggin
#   cat("---- n -----\n")
#   print(n)
#   cat("---- over.n -----\n")
#   print(over.n)
#   cat("---- strat.var -----\n")
#   print(strat.var)
#   cat("---- strata.levels -----\n")
#   print(strata.levels) 
#   cat("---- head(shp) -----\n")
#   print(head(data.frame(shp)))

  #this makes a list of elements to be passed to the grts function
  selType="Equal"
  Stratdsgn <- lapply(1:length(strata.levels), function(x, nn, st, o.n){
    list(panel=c(Main=n[x]),seltype=selType,over=over.n)
  }, nn=n, st=selType, o.n=over.n)
  names(Stratdsgn) <- strata.levels
 

# ------------- PRINT TO CONSOLE ----------------------------------------------------------------
# prepare stratum string for printing
for(i in 1:length(strata.levels)){
  if(i == 1){
    string <- paste("c(",dQuote(strata.levels[1]),sep="")
  } else {
    string <- paste(string,",",dQuote(strata.levels[i]),sep="")
  }
}
string <- paste(string,")",sep="")

# prepare n-string for printing
for(i in 1:length(n)){
  if(i == 1){
    nstring <- paste("c(",n[1],sep="")
  } else {
    nstring <- paste(nstring,",",n[i],sep="")
  }
}
nstring <- paste(nstring,")",sep="")  

cat("# Prepare the design of the sampling for use in the grts function.\n
  n <- ",nstring,"\n
  Stratdsgn <- lapply(1:length(",string,"), function(x, nn, st, o.n){
        list(panel=c(Main=n[x]),seltype=",dQuote(get("selType")),",over=",get("over.n"),")
      }, nn=",nstring,", st=",dQuote(get("selType")),", o.n=",get("over.n"),")\n
  names(Stratdsgn) <- ",string,"\n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------







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



# ------------- PRINT TO CONSOLE ----------------------------------------------------------------
cat("# Draw the sample via the grts function in package spsurvey.\n
      Stratsites <- grts(design=Stratdsgn,
      DesignID='STRAT',
    type.frame=",dQuote(get("sframe.type")),",
    att.frame=data.frame(shp),
    src.frame='sp.object',
    sp.object=shp,
    stratum=",dQuote(get("strat.var")),",
    shapefile=FALSE)
    \n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------



    Stratsites <- grts(design=Stratdsgn,
            DesignID="STRAT",
            type.frame=sframe.type,
            att.frame=data.frame(shp),
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
    attr(Stratsites, "strata.var") <- "stratum"

    makeLog(strat.var=strat.var,strata.levels=strata.levels,unequal.var=NULL,alloc.type=NULL,category.levels=NULL,n,over.n,shp,fn,dir,outobj,sframe.type=sframe.type,selType=selType)

    Stratsites
}